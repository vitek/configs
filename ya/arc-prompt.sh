#!/bin/bash
# arc --set-channel prestable && arc --set-channel stable
__arc_ps1 () {
    local exit=$?
    case "$#" in
        0) :
        ;;
        *)  return $exit
        ;;
    esac
    local arc
    local repo_info
    arc=$(which ~/.arc/current/arc arc 2> /dev/null | head -1)
    if [ "x$arc" = "x" ]; then
        return $exit
    fi
    repo_info=$($arc info --json 2>/dev/null)
    if [ -z "$repo_info" ]; then
        return $exit
    fi
    local branch_info=$(echo $repo_info | jq -r '.branch')
    printf -- "(%s) " "$branch_info"
    return $exit
}
