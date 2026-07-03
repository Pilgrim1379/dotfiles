# --- GitHub Actions run helpers -------------------------------------------

# Track recently used repos for tab-completion
_gh_repo_cache="${HOME}/.cache/gh_repos_history"

_gh_remember_repo() {
  mkdir -p "$(dirname "$_gh_repo_cache")"
  # dedupe, keep most recent 20
  { echo "$1"; [[ -f "$_gh_repo_cache" ]] && cat "$_gh_repo_cache"; } \
    | awk '!seen[$0]++' | head -20 > "${_gh_repo_cache}.tmp" \
    && mv "${_gh_repo_cache}.tmp" "$_gh_repo_cache"
}

# List all runs for a repo
ghruns() {
  [[ -z "$1" ]] && { echo "Usage: ghruns <owner/repo>"; return 1; }
  _gh_remember_repo "$1"
  gh run list --repo "$1"
}

# List only in-progress runs
ghrunsip() {
  [[ -z "$1" ]] && { echo "Usage: ghrunsip <owner/repo>"; return 1; }
  _gh_remember_repo "$1"
  gh run list --repo "$1" --status in_progress
}

# List only queued runs
ghrunsq() {
  [[ -z "$1" ]] && { echo "Usage: ghrunsq <owner/repo>"; return 1; }
  _gh_remember_repo "$1"
  gh run list --repo "$1" --status queued
}

# List in-progress + queued together (i.e. "stuck" candidates)
ghrunstuck() {
  [[ -z "$1" ]] && { echo "Usage: ghrunstuck <owner/repo>"; return 1; }
  _gh_remember_repo "$1"
  echo "--- in_progress ---"
  gh run list --repo "$1" --status in_progress
  echo "--- queued ---"
  gh run list --repo "$1" --status queued
}

# Cancel a run: graceful cancel, fall back to force-cancel
ghcancel() {
  if [[ -z "$1" || -z "$2" ]]; then
    echo "Usage: ghcancel <owner/repo> <run_id>"
    return 1
  fi
  local repo="$1" id="$2"
  _gh_remember_repo "$repo"

  echo "Attempting graceful cancel of $id..."
  if gh run cancel "$id" --repo "$repo" 2>/dev/null; then
    echo "Cancelled."
    return 0
  fi

  echo "Graceful cancel failed, trying force-cancel..."
  if gh api --method POST \
    -H "Accept: application/vnd.github+json" \
    -H "X-GitHub-Api-Version: 2022-11-28" \
    "/repos/${repo}/actions/runs/${id}/force-cancel"; then
    echo "Force-cancelled."
  else
    echo "Force-cancel also failed — run may be orphaned. Consider an empty commit + push to supersede it."
  fi
}

gpecommit(){
  git commit --allow-empty -m "Trigger fresh Pages deploy" && git push
}

# --- Tab completion for owner/repo argument --------------------------------
_gh_repo_complete() {
  local -a repos
  [[ -f "$_gh_repo_cache" ]] && repos=("${(@f)$(cat "$_gh_repo_cache")}")
  _describe 'repo' repos
}

compdef _gh_repo_complete ghruns
compdef _gh_repo_complete ghrunsip
compdef _gh_repo_complete ghrunsq
compdef _gh_repo_complete ghrunstuck
compdef _gh_repo_complete ghcancel