// compile your app with:
//		elm make YourApp.elm --output elm.js
// run your app with:
//		node main

const fs = require('fs');
const path = require('path');


const elmViewModule = process.argv[2];
if (typeof(elmViewModule) == "undefined") {
	console.log("You need to supply the path to an Elm module. It should have at least one view function in it.");
	process.exit(1);
}

const elmFlags = {
	elmPackageContents: fs.readFileSync('elm-package.json', "utf-8"),
  viewModuleContents: fs.readFileSync(elmViewModule, "utf-8"),
}


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

ports.readElmModule.subscribe(data => {
	console.log('read elm module');

	fs.readFile(data.path, 'utf8', (err, contents) => {
	  if (err) {
			ports.readElmModuleResult.send({ scope: data.scope, contents: null});
		} else {
			ports.readElmModuleResult.send({ scope: data.scope, contents: contents});
		}
	});
});

ports.getFilenamesInDir.subscribe(function getFilenamesInDir(data) {
	console.log('data', data);
	const scope = data.scope;
	const path = data.path;
	fs.readdir(path, (err, files) => {
			if (err) {
				ports.getFilenamesInDirResult.send({filenames: [], scope: scope});
			} else {
				console.log('success');
				ports.getFilenamesInDirResult.send({filenames: files, scope: scope});
			}
	});
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



// function readElmFiles(dirs, elmFiles) {
// 	elmFiles.forEach(function(elmFile) {
// 		const path = elmFile.path;
// 		dirs.map(tryOpeningFile
//
// 	});
// }
//
// function tryOpeningFile(dir, path) {
// 	try {
// 		return fs.readFileSync(path.join(dir, path));
// 	} catch (error) {
// 		return null;
// 	}
// }
