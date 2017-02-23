#!/bin/zsh

# schedule template file
day=$(date +%A | tr '[:upper:]' '[:lower:]')
template_dir="$PERSONAL/schedule/daily/templates"
template_file="$template_dir/$day"

# destination schedule file
date_str=$(date +'%Y-%m-%d')
schedule_dir="$PERSONAL/schedule/daily"
schedule_file="$schedule_dir/$date_str.md"

cp -n "$template_file" "$schedule_file"

