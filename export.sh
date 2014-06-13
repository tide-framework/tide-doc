#!/bin/bash

./pillar export --to='html' 'documentation.pier' > documentation.html
./pillar export --to='latex' 'documentation.pier' > documentation.tex

