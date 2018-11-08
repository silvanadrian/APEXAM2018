#!/bin/bash
# removes all ignored files in git from an archive
git archive -o handin.zip --prefix handin/ HEAD:handin
