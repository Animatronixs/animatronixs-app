/**
 * Install on Tessel2 by running the following command 
 * from inside the 'dist' directory:
 * t2 run server_tessel2.js
 * 
 * Connect to the access point 'Animatronixs-*'
 * 
 * Then open a browser page at
 * http://192.168.1.101:8080/
 */

var tessel = require('tessel');
var http = require('http');
var fs = require('fs');
var url = require('url');

var foo = require('./foo'); // FOR TEST ONLY!!!
var bar = require('./bar'); // FOR TEST ONLY!!!

var server = http.createServer(function (request, response) {
  // Break up the url into easier-to-use parts
  var urlParts = url.parse(request.url, true);
  switch (request.url) {
    case "/stylesheet.css" :
      returnStylesheetCSS(request, response);
      break;
    case "/client.js" :
      returnClientJS(request, response);
      break;
    case "/favicon.ico" :
      returnFaviconIco(request, response);
      break;  
    case "/" :  
      returnIndexHTML(request, response);
      break;
    default :
      returnIndexHTML(request, response);
  };
});

server.listen(8080);
console.log("Connect to WiFi access point 'Animatronixs-*'");
console.log("Server running at http://192.168.1.101:8080/");

function returnIndexHTML (request, response) {
  response.writeHead(200, {"Content-Type": "text/html"});
  fs.readFile(__dirname + '/index.html', function (err, content) {
    if (err) {
      throw err;
    }
    response.end(content);
  });
}

function returnStylesheetCSS (request, response) {
  response.writeHead(200, {"Content-Type": "text/css"});
  fs.readFile(__dirname + '/stylesheet.css', function (err, content) {
    if (err) {
      throw err;
    }
    response.end(content);
  });
}

function returnClientJS (request, response) {
  response.writeHead(200, {"Content-Type": "application/javascript"});
  fs.readFile(__dirname + '/client.js', function (err, content) {
    if (err) {
      throw err;
    }
    response.end(content);
  });
}

function returnFaviconIco (request, response) {
  response.writeHead(200, {"Content-Type": "image/x-icon"});
  fs.readFile(__dirname + '/favicon.ico', function (err, content) {
    if (err) {
      // throw err;
      // images can not be found on Tessel2 momentarily, so just continue instead
      response.end();
    }
    response.end(content);
  });
}