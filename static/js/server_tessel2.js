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
  var fileExtension = urlWithoutParams.split('.').pop();
  var contentType = "";
  switch(fileExtension.toUpperCase()) {
    case "CSS":
      contentType = "text/css";
      returnAsset(request, response, contentType)
      break; 
    case "EOT":
      contentType = "application/vnd.ms-fontobject";
      returnAsset(request, response, contentType)
      break;
    case "HTML":
      contentType = "text/html";
      returnAsset(request, response, contentType)
      break; 
    case "ICO":
      contentType = "image/x-icon";
      returnAsset(request, response, contentType)
      break; 
    case "JPEG":
      contentType = "image/jpeg";
      returnAsset(request, response, contentType)
      break;                    
    case "JPG":
      contentType = "image/jpeg";
      returnAsset(request, response, contentType)
      break;
    case "JS":
      contentType = "application/javascript";
      returnAsset(request, response, contentType)
      break;      
    case "PNG":
      contentType = "image/png";
      returnAsset(request, response, contentType)
      break;
    case "SVG":
      contentType = "image/svg+xml";
      returnAsset(request, response, contentType)
      break;      
    case "TTF":
      contentType = "font/ttf";
      returnAsset(request, response, contentType)
      break; 
    case "WOFF":
      contentType = "font/woff";
      returnAsset(request, response, contentType)
      break;
    case "XML":
      contentType = "text/xml";
      returnAsset(request, response, contentType)
      break;                     
    default :
      // DO NOTHING
  }

  switch (urlWithoutParams) {  
    case "/toggleleds" : 
      returnToggleLeds(request, response);
      break;
    case "/rotateservos" : 
      returnRotateServos(request, response);
      break;                
    case "/" : 
      request.url = "/index.html";
      contentType = "text/html";
      returnAsset(request, response, contentType);
      break;
    default :
      // DO NOTHING
  };
});

server.listen(8080);
console.log("Connect to WiFi access point 'Animatronixs-*'");
console.log("Server running at http://192.168.1.101:8080/");

function returnAsset (request, response, contentType) {
  response.writeHead(200, {"Content-Type": contentType});
  fs.readFile(__dirname + request.url, function (err, content) {
    if (err) {
      //throw err;
      console.log(err);
      response.writeHead(500, {"Content-Type": "application/json"});
      response.end(JSON.stringify({asset: request.url, error: err}));
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

function returnRotateServos (request, response) {
  // Create a regular expression to find the number at the end of the url
  var indexRegex = /(\d+)$/;

  // Capture the number, returns an array
  var result = indexRegex.exec(request.url);

  if(result === null) {
    console.log("index is null");
    response.writeHead(500, {"Content-Type": "application/json"});
    response.end(JSON.stringify({servoId: null, error: "index is null"}));
  }
  else {
    // Grab the captured result from the array
    var index = result[1];
    var servolib = require('servo-pca9685');
    var servo = servolib.use(tessel.port['A']);
    var servoId = 1; // We have a servo plugged in at position 1, make it a dynamic assignment by parameter
    // NOTE: index is between 0 and 100
    var position = index == 0 ? 0 : (index/100); //  Target position of the servo between 0 (min) and 1 (max).

    servo.on('ready', function () {
      if(position < 0 || position > 1) // Make sure the target position of the servo is between 0 (min) and 1 (max), 
                                       // otherwise it may cause damage to the servo
      {
        console.log("servo target position is not between 0 (min) and 1 (max): ", position);
        response.writeHead(500, {"Content-Type": "application/json"});
        response.end(JSON.stringify({servoId: servoId, error: "target position  is not between 0 (min) and 1 (max)"}));
      }
      else //  Target position of the servo between 0 (min) and 1 (max).
      {
        //  Set the minimum and maximum duty cycle for servo.
        //  If the servo doesn't move to its full extent or stalls out
        //  and gets hot, try tuning these values (0.05 and 0.12).
        //  Moving them towards each other = less movement range
        //  Moving them apart = more range, more likely to stall and burn out
        servo.configure(servoId, 0.05, 0.12, function () {
          // console.log('Position (in range 0-1):', position);
          // Set servo to position
          servo.move(servoId, position);
        });
        console.log("servo target position is between 0 (min) and 1 (max): ", position);
        response.writeHead(200, {"Content-Type": "application/json"});
        response.end(JSON.stringify({servoId: servoId, position: position}));
      }
    });
  }
}