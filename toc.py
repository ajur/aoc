#!/usr/bin/env python3

import os
import re

langs = {
  'js': 'JavaScript',
  'ts': 'TypeScript',
  'hs': 'Haskell'
}

def find_files_starting_with_day(root_dir='.'):
  pattern = re.compile(r'.*/aoc(\d+)/.*day(\d+).*\.(js|ts|hs)$')
  for root, dirs, files in os.walk(root_dir):
    for file in files:
      file_path = os.path.join(root, file)
      match = pattern.match(file_path)
      if match:
        aoc = match.group(1)
        day = int(match.group(2))
        ext = match.group(3)
        yield (aoc, day, ext, file_path)

def map_to_links(files):
  prevAoc = 0
  for aoc, day, ext, path in sorted(find_files_starting_with_day(), reverse=True):
    aocLink = f' [{aoc}](https://adventofcode.com/{aoc}) '
    dayLink = f' [day {day}](https://adventofcode.com/{aoc}/day/{day}) '
    solPath = f' [{langs[ext]}]({path}) '
    yield (aocLink if prevAoc != aoc else '', dayLink, solPath)
    prevAoc = aoc

def max_lengths(links):
  max_len = (0, 0, 0)
  for link in links:
    max_len = tuple(max(max_len[i], len(link[i])) for i in range(3))
  return max_len

def to_rows(links, maxl):
  ml1, ml2, ml3 = maxl
  return [f"|{l1.ljust(ml1)}|{l2.ljust(ml2)}|{l3.ljust(ml3)}|" for l1, l2, l3 in links]

def separator(maxl):
  return f"|{'-' * maxl[0]}|{'-' * maxl[1]}|{'-' * maxl[2]}|"

if __name__ == "__main__":
  files = find_files_starting_with_day()
  links = list(map_to_links(files))
  maxl = max_lengths(links)
  rows = to_rows(links, maxl)
  # print('\n'.join(rows))
  with open('TOC.md', 'w') as file:
    file.write(to_rows([("Year", "Day", "Code")], maxl)[0])
    file.write('\n')
    file.write(separator(maxl))
    file.write('\n')
    file.write('\n'.join(rows))
    file.write('\n')
