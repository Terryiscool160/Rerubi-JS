/*
    Module runner
*/

const Rerubi = require('./Source')
const FS = require('fs')

const Function = Rerubi(FS.readFileSync('Out.out', 'binary'))
Function()