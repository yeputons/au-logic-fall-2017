#!/bin/bash
(for i in */log.md; do
  echo "# $i"
  cat $i
done) > logs.md
