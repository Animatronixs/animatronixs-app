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
  var urlWithoutParams = request.url;
  if(urlWithoutParams.indexOf('?') != -1) {
    // ignore '?...' from request.url
    urlWithoutParams = urlWithoutParams.substring(0, urlWithoutParams.indexOf('?'));
  }
  switch (urlWithoutParams) {
    case "/stylesheet.css" :
      returnStylesheetCSS(request, response);
      break;
    case "/client.js" :
      returnClientJS(request, response);
      break;
    case "/favicon.ico" :
      returnFaviconIco(request, response);
      break; 
    case "/toggleleds" : 
      returnToggleLeds(request, response);
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

function returnToggleLeds (request, response) {
  // Create a regular expression to find the number at the end of the url
  var indexRegex = /(\d+)$/;

  // Capture the number, returns an array
  var result = indexRegex.exec(request.url);

  if(result === null) {
    console.log("index is null");
    response.writeHead(500, {"Content-Type": "application/json"});
    response.end(JSON.stringify({ledId: null, error: "index is null"}));
  }
  else {
    // Grab the captured result from the array
    var index = result[1];    

    // Use the index to reference the correct LED
    var led = tessel.led[index];

    if(typeof led === 'undefined') {
      console.log("led is undefined");
      response.writeHead(500, {"Content-Type": "application/json"});
      response.end(JSON.stringify({ledId: parseInt(index), error: "led is undefined"}));
    }
    else if(led === null) {
      console.log("led is null");
      response.writeHead(500, {"Content-Type": "application/json"});
      response.end(JSON.stringify({ledId: null, error: "led is null"}));
    }
    else {
      // Toggle the state of the led and call the callback after that's done
      led.toggle(function (err) {
        if (err) {
          // Log the error, send back a 500 (internal server error) response to the client
          console.log(err);
          response.writeHead(500, {"Content-Type": "application/json"});
          response.end(JSON.stringify({ledId: parseInt(index), error: err}));
        } else {
          // The led was successfully toggled, respond with the state of the toggled led using led.isOn
          response.writeHead(200, {"Content-Type": "application/json"});
          response.end(JSON.stringify({ledId: parseInt(index), on: led.isOn}));
        }
      });
    }
  }
}