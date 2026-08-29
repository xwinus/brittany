#!/bin/sh

set -eu

project_root=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
cd "$project_root"

manifest=data/maintained-source-files.txt
jobs=${BRITTANY_JOBS:-4}

case $jobs in
  ''|*[!0-9]*|0)
    echo "BRITTANY_JOBS must be a positive integer" >&2
    exit 2
    ;;
esac

if [ "${1:-}" = "--worker" ]; then
  index=$2
  source_file=$3
  formatter=$4
  work_directory=$5
  output_file="$work_directory/$index.output"
  status_file="$work_directory/$index.status"
  if "$formatter" \
      --no-user-config \
      --config-file data/brittany.yaml \
      --dump-fallbacks-json \
      --fail-on-fallback \
      --suppress-output \
      "$source_file" >"$output_file" 2>&1; then
    exit_code=0
  else
    exit_code=$?
  fi
  {
    printf 'FILE %s\n' "$source_file"
    cat "$output_file"
    printf '\n'
  } >"$work_directory/$index.report"
  printf '%s\n' "$exit_code" >"$status_file"
  exit 0
fi

formatter=${1:-}
if [ -z "$formatter" ]; then
  formatter=$(cabal list-bin exe:brittany)
fi

if [ ! -x "$formatter" ]; then
  echo "formatter is not executable: $formatter" >&2
  exit 2
fi

work_directory=$(mktemp -d "${TMPDIR:-/tmp}/brittany-fallback-inventory.XXXXXX")
trap 'rm -rf "$work_directory"' EXIT HUP INT TERM

find doc-svg-gen source/executable source/library source/test-suite \
  -type f -name '*.hs' \
  ! -path 'source/test-suite/fixtures/*' \
  -print \
  | LC_ALL=C sort >"$work_directory/discovered"

if ! diff -u "$manifest" "$work_directory/discovered"; then
  echo "maintained-source manifest is out of date" >&2
  exit 2
fi

index=0
running=0
pids=''
while IFS= read -r source_file; do
  padded_index=$(printf '%06d' "$index")
  "$0" --worker "$padded_index" "$source_file" "$formatter" \
    "$work_directory" &
  pids="$pids $!"
  running=$((running + 1))
  index=$((index + 1))
  if [ "$running" -eq "$jobs" ]; then
    for pid in $pids; do
      wait "$pid"
    done
    running=0
    pids=''
  fi
done <"$manifest"

for pid in $pids; do
  wait "$pid"
done

exit_code=0
index=0
while IFS= read -r source_file; do
  padded_index=$(printf '%06d' "$index")
  cat "$work_directory/$padded_index.report"
  worker_status=$(cat "$work_directory/$padded_index.status")
  if [ "$worker_status" -ne 0 ]; then
    exit_code=$worker_status
  fi
  index=$((index + 1))
done <"$manifest"

exit "$exit_code"
