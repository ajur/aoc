#!/usr/bin/env python3

import os
import re
from collections import defaultdict

def find_files_starting_with_day(root_dir='.'):
  pattern = re.compile(r'./([\w-]+)/(\d+)/.*[dD]ay(\d+).*\.(js|ts|hs)$')
  
  for root, dirs, files in os.walk(root_dir):
    for file in files:
      file_path = os.path.join(root, file)
      match = pattern.match(file_path)
      if match:
        aoc = int(match.group(2))
        day = int(match.group(3))
        lang = match.group(1)
        yield (aoc, day, lang, file_path)

def groupByYear(files):
  by_year = defaultdict(list)
  for file in files:
    by_year[file[0]].append(file)
  return [(year, sorted(items, reverse=True)) for year, items in sorted(by_year.items(), reverse=True)]

def map_to_row(files):
  for year, days in files:
    tdYear = f'<td rowspan="{len(days)}" style="vertical-align: top"><a href="https://adventofcode.com/{year}">{year}</a></td>'
    for aoc, day, lang, path in days:
      tdDay = f'<td><a href="https://adventofcode.com/{year}/day/{day}">day {day}</a></td>'
      tdFile = f'<td><a href={path}>{lang}</a></td>'
      yield f'    <tr>{tdYear}{tdDay}{tdFile}</tr>'
      tdYear = '';

TABLE = '''
# Table of contents

<table>
  <thead>
    <tr><th>Year</th><th>Day</th><th>Solution source</th></tr>
  </thead>
  <tbody>
{ROWS}
  </tbody>
</table>
'''


if __name__ == "__main__":
  files = list(find_files_starting_with_day())
  links = groupByYear(files)
  rows = map_to_row(links)
  
  with open('README.md', 'w') as readme:
    with open('info.md', 'r') as info:
      readme.write(info.read())
      readme.write('\n\n')
      readme.write(TABLE.replace('{ROWS}', '\n'.join(rows)))
