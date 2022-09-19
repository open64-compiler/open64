//
// Copyright (C) 2020 Xcalibyte Limited, Inc.  All Rights Reserved
//

// before run the case, need to install ref-napi and ffi-napi
// $ npm install ref-napi
// $ npm install ffi-napi


// copy wffi.js from xvsa install library directory
// either to modify the libwffi.so path in wffi.js, or
// export LD_LIBRARY_PATH=<xvsa-install-library-directory>

// import wffi moodule
var whirl = require('./wffi.js');
console.log("load library succ: " + whirl);

// initialize the whirl generator
whirl.Wgen_Initialize('input.c', 'output.B', 'n64');
console.log("called Wgen_Initialize");

// finalize whirl generator. output.B should be generated
whirl.Wgen_Finalize();
console.log("called Wgen_Finalize");
