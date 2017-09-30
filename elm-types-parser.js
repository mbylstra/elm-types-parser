#!/usr/bin/env node

const fs = require('fs');
const path = require('path');

const subjectSourceCodePath = process.argv[2];
if (typeof subjectSourceCodePath == 'undefined') {
  console.log(
    'You need to supply the path to an Elm module. It should have at least one view function in it.'
  );
  process.exit(1);
}

const elmFlags = {
  elmPackageContents: fs.readFileSync('elm-package.json', 'utf-8'),
  subjectSourceCode: fs.readFileSync(subjectSourceCodePath, 'utf-8'),
  exactDependenciesContents: fs.readFileSync(
    'elm-stuff/exact-dependencies.json',
    'utf-8'
  ),
};

// load Elm module
const elm = require('./elm.js');

// get Elm ports
const ports = elm.Main.worker(elmFlags).ports;

// keep our app alive until we get an exitCode from Elm or SIGINT or SIGTERM (see below)
setInterval(id => id, 86400);

ports.exitApp.subscribe(exitCode => {
  console.log('Exit code from Elm:', exitCode);
  process.exit(exitCode);
});

var moduleAttempts = new Map();

ports.readElmModule.subscribe(data => {
  var returnedData = null;

  // modulesAttempted.add(data.portScope.moduleName);

  var moduleName = data.portScope.moduleName;
  if (moduleAttempts.has(moduleName)) {
    moduleAttempts.set(moduleName, moduleAttempts.get(moduleName) + 1);
  } else {
    moduleAttempts.set(moduleName, 1);
  }

  if (fs.existsSync(data.path)) {
    // fs.readFile(data.path, 'utf8', (err, contents) => {
    //   if (err) {
    //     console.log(err);
    //     throw err;
    //     // ports.readElmModuleResult.send({
    //     //   portScope: data.portScope,
    //     //   contents: null,
    //     // });
    //   } else {
    //     ports.readElmModuleResult.send({
    //       portScope: data.portScope,
    //       contents: contents,
    //     });
    //   }
    // });
    var contents = fs.readFileSync(data.path, 'utf8');
    returnedData = {
      portScope: data.portScope,
      maybeContents: contents,
    };
  } else {
    // console.log('File not found');
    returnedData = {
      portScope: data.portScope,
      maybeContents: null,
    };
  }
  printReturnedDataInfo(returnedData);
  console.log('modules attempted', moduleAttempts);
  ports.readElmModuleResult.send(returnedData);
});

function printReturnedDataInfo(returnedData) {
  console.log('JS portScope', returnedData.portScope);
  console.log('JS foundModule/hasContents', returnedData.contents != null);
}

ports.readElmPackageInfoContents.subscribe(elmPackageJsonPaths => {
  ports.readElmPackageInfoContentsResult.send(
    elmPackageJsonPaths.map(path => [path, fs.readFileSync(path, 'utf8')])
  );
});

ports.writeOutput.subscribe(output => {
  console.log('write output??');
  console.log(output);
});

process.on('uncaughtException', err => {
  console.log(`Uncaught exception:\n`, err);
  process.exit(1);
});

process.on('SIGINT', _ => {
  console.log(`SIGINT received.`);
  ports.externalStop.send(null);
});

process.on('SIGTERM', _ => {
  console.log(`SIGTERM received.`);
  ports.externalStop.send(null);
});
