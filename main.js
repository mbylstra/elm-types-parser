// compile your app with:
//		elm make YourApp.elm --output elm.js
// run your app with:
//		node main

const fs = require('fs');

// load Elm module
const elm = require('./elm.js');

const flags = {
	elmPackageContents: fs.readFileSync('elm-package.json', "utf-8")
}
// get Elm ports
const ports = elm.Main.worker(flags).ports;

// keep our app alive until we get an exitCode from Elm or SIGINT or SIGTERM (see below)
setInterval(id => id, 86400);

ports.exitApp.subscribe(exitCode => {
	console.log('Exit code from Elm:', exitCode);
	process.exit(exitCode);
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
