var path = require('path');

exports.build = path.dirname(process.argv[1]);

exports.cache = path.join(exports.build, 'cache');
exports.project = path.join(exports.build, '..');

exports.target = path.join(exports.project, 'target/');
exports.source = path.join(exports.project, 'src');

