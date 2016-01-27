#!/usr/bin/env node

require('shelljs/global');
var directories = require('./directories.js');
var path = require('path');

rm('-Rf', directories.target);

// cp('-R', directories.site, directories.target);

cd(directories.build);

exec('elm-make ' + directories.source + '/Main.elm' + ' --output ' + directories.target + '/index.html' );
