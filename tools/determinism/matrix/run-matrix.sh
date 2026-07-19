#!/usr/bin/env bash
# Multi-libcurl probe matrix driver (RURL-ttrcfneq).
#
# Builds one image per distro, runs tools/determinism/curl-probe.R inside each,
# and copies curl-<LABEL>.csv / curlenv-<LABEL>.csv back into
# tools/determinism/out/ (gitignored). Re-runnable and idempotent: re-running
# overwrites the same per-label files. No secrets; the only network needed is
# the distro package archive.
#
#   tools/determinism/matrix/run-matrix.sh              # whole matrix
#   tools/determinism/matrix/run-matrix.sh ubuntu2204   # selected images only
#
# An image that refuses to provision is REPORTED AND SKIPPED, never fatal: the
# point is version spread, and one stubborn EOL distro must not block the
# sweep. The run summary at the end lists ok/failed per image.
set -u

here="$(cd "$(dirname "$0")" && pwd)"
det_dir="$(cd "${here}/.." && pwd)"
out_dir="${det_dir}/out"
ctx="$(mktemp -d)"
trap 'rm -rf "${ctx}"' EXIT

mkdir -p "${out_dir}"
cp "${det_dir}/corpus.csv" "${det_dir}/curl-probe.R" \
   "${here}/entrypoint.sh" "${here}/Dockerfile" "${ctx}/"

# name|base image|expected libcurl (label only, never asserted)|apt preamble
#
# Spread is the goal, not these exact images. curl_url() -- the API
# curl_parse_url() wraps -- landed in libcurl 7.62.0 (2018), so the sample
# starts just above that and runs to current.
#
# ubuntu:20.04 is EOL and no longer resolves on its live archive host; the apt
# preamble repoints sources at old-releases. Two other EOL candidates were
# tried and DROPPED (time-boxed, see the unit worklog): debian:11 and
# ubuntu:23.10 both reach their archive mirror but cannot satisfy
# build-essential + libcurl4-openssl-dev ("held broken packages"). They would
# have added libcurl 7.74 and 8.2; the sweep has enough spread without them.
EOL_UBUNTU='sed -i -e s/archive.ubuntu.com/old-releases.ubuntu.com/g -e s/security.ubuntu.com/old-releases.ubuntu.com/g /etc/apt/sources.list'

IMAGES=(
  "ubuntu2004|ubuntu:20.04|7.68|${EOL_UBUNTU}"
  "ubuntu2204|ubuntu:22.04|7.81|true"
  "debian12|debian:12|7.88|true"
  "ubuntu2404|ubuntu:24.04|8.5|true"
  "debian13|debian:13|8.14|true"
)

want=("$@")
selected() {
  [ ${#want[@]} -eq 0 ] && return 0
  local w
  for w in "${want[@]}"; do [ "$w" = "$1" ] && return 0; done
  return 1
}

ok_list=()
fail_list=()

for spec in "${IMAGES[@]}"; do
  IFS='|' read -r name base expect pre <<<"${spec}"
  selected "${name}" || continue

  tag="rurl-determinism:${name}"
  echo "=============================================================="
  echo "[${name}] base=${base} expected libcurl ~${expect}"
  echo "=============================================================="

  if ! docker build --pull \
        --build-arg "BASE_IMAGE=${base}" \
        --build-arg "APT_PRE=${pre}" \
        -t "${tag}" "${ctx}"; then
    echo "[${name}] BUILD FAILED -- skipping"
    fail_list+=("${name} (build)")
    continue
  fi

  cname="rurl-det-${name}-$$"
  docker rm -f "${cname}" >/dev/null 2>&1
  if ! docker run --name "${cname}" \
        -e "RURL_DETERMINISM_LABEL_PREFIX=${name}" \
        "${tag}"; then
    echo "[${name}] RUN FAILED -- skipping"
    docker rm -f "${cname}" >/dev/null 2>&1
    fail_list+=("${name} (run)")
    continue
  fi

  if docker cp "${cname}:/out/." "${out_dir}/"; then
    ok_list+=("${name}")
  else
    echo "[${name}] COPY-OUT FAILED"
    fail_list+=("${name} (copy)")
  fi
  docker rm -f "${cname}" >/dev/null 2>&1
done

echo
echo "==================== matrix summary ====================="
echo "provisioned: ${ok_list[*]:-<none>}"
echo "skipped:     ${fail_list[*]:-<none>}"
echo
echo "collected artifacts in ${out_dir}:"
ls -1 "${out_dir}" | grep -E '^curl(env)?-' || true
echo
echo "next: Rscript tools/determinism/matrix/divergence.R"
