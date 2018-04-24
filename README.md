# Animatronixs - App

## Renaming your Tessel 2

The following will rename your Tessel 2 to ```Animatronixs-Snowy-Owl```:

```javascript
t2 rename Animatronixs-Snowy-Owl
```

Check if the renaming has been successful as follows:

```javascript
t2 list
INFO Searching for nearby Tessels...
        USB     Animatronixs-Snowy-Owl
```

## Renaming Your Tessel 2 On The Wi-Fi Network

The following will make an open, or password-less, wifi network called Animatronixs-Snowy-Owl.

```javascript
t2 ap -n Animatronixs-Snowy-Owl
```

You will be prompted as follows:

```javascript
node-pre-gyp INFO Looking for your Tessel...
node-pre-gyp INFO Connected to Animatronixs-Snowy-Owl.
node-pre-gyp INFO Updated Access Point successfully. SSID: Animatronixs-Snowy-Owl
```

Check if the renaming has been successful as follows:

```javascript
t2 list
INFO Searching for nearby Tessels...
        USB     Animatronixs-Snowy-Owl
        LAN     Animatronixs-Snowy-Owl
```

Open the wifi setting of your computer, or a separate device like a smartphone or tablet, and scan for new devices to see and connect to this new network.

Create an network that requires a password. (Hint: for information about making a secure network, take a look at the [cli docs](https://tessel.io/docs/cli#usage).)

## Build A Portable Web Server

Most web applications are served from remote machines in data centers miles and miles away from the devices that are using them. Because we're used to high-speed internet connections, that distance can seem trivial and unnoticeable.

The Tessel 2 can run a server and deliver web applications to locally connected devices. It's quicker than those remote machines due to the immediate distance between the Tessel and devices connected to it. As part of this project we'll start a server on the Tessel and serve a web application that controls the Tessel through a local network.

## Get Started

```bash
git clone git@github.com:animatronixs/animatronixs-app.git
cd animatronixs-app

# Install bower
npm install -g bower

# Install yarn
npm install -g yarn

# install nvm for windows following these instructions
https://github.com/coreybutler/nvm-windows

# After installing, set nvm in environment variable NVM_HOME, e.g.
# C:\Users\user\AppData\Roaming\nvm\nvm.exe

# Use nvm to see the node versions installed
nvm list

# Set the node version to use (e.g. 8.10.0).
nvm use 8.10.0

# Install npm, and yarn.
yarn run installPackages

# Make sure the 'dist' directory is deleted before running below script
yarn run buildDist
```

On Linux:
```bash
xdg-open dist/index.html
```

On Windows:
```bash
start .\dist\index.html
```

To run the project (currently up-to-date for Purescript 0.11.7):

```
# Follow the instructions how to install Node-Gyp on 
# https://github.com/nodejs/node-gyp
# as administrator
> npm install --global --production windows-build-tools

# If you have NOT installed Node-Gyp as described above, then
# Install Python 2.* 
# and add 'python2' in the environment variable PATH

# If you have NOT installed Node-Gyp as described above, then
# Install Windows SDK
# See https://developer.microsoft.com/en-us/windows/downloads/sdk-archive 
# as it is required by Visual Studio 2015, when installing node-sass, 
# and not provided by default

# Install node-sass@4.5.3
# It is now added to package.json

# Update npm to a version >= 5.7.0
> npm -g install npm

# Install dependencies
> bower install
> npm install

# Build
> npm run build

# Serve
> npm start

# Open the index page
> http://localhost:8080/
```

Call the Server's REST API as follows with e.g. Postman:

```javascript
GET http://localhost:8080/getorders?params=2
```

It will return:

```javascript
[{"values":[{"quantity":6,"productId":2}],"tag":"EndpointExample.Model.Order"}]
```

Install and run the Animatronixs web app on the Tessel 2:

```javascript
cd dist
t2 run server_tessel2.js
```

The Server now runs on the Tessel 2 and listens to http://192.168.1.101 on WiFi Access Point "Animatronixs-Snowy-Owl"

Want to untether your computer from your Tessel? Run:

```javascript
cd dist
t2 push server_tessel2.js
```

You will be prompted with:

```javascript
INFO Looking for your Tessel...
INFO Connected to Animatronixs-Snowy-Owl.
INFO Building project.
INFO Writing project to Flash on Animatronixs-Snowy-Owl (4291.072 kB)...
INFO Deployed.
INFO Your Tessel may now be untethered.
INFO The application will run whenever Tessel boots up.
INFO      To remove this application, use "t2 erase".
INFO Running server_tessel2.js...
```

You can now unplug the USB cable from the Tessel 2 to your computer. Instead connect the USB cable from Tessel 2 to a battery pack. The Tessel 2's access point will still be available, whereas the Tessel 2 is now truly wireless.

TO DO: 
The Server should apply the LED id to toggle the GREEN LED which has ID 2 on the Tessel 2.

TO DO:
Make it so that we can call the Server's REST API with:

```javascript
GET http://localhost:8080/leds/2

TO DO:
Rewrite Server.purs so it includes the logic of server_tessels.js to toggle LEDs on the Tessel 2. Later include a REST API to manipulate a Servo motor.

References:

"Type-safe client-server communication with PureScript" at 
[https://frigoeu.github.io/phantomtypes.html](https://frigoeu.github.io/phantomtypes.html)

And accompanying example at [https://github.com/FrigoEU/purescript-endpoints-express-example](https://github.com/FrigoEU/purescript-endpoints-express-example)

"Halogen Menu" at [https://github.com/slamdata/purescript-halogen-menu](https://github.com/slamdata/purescript-halogen-menu)

What to do when the Tessel 2 is not detected.

First and foremost try unconnecting the USB cable of the Tessel 2 from the PC then reconnecting the USB cable of the Tessel 2 to the PC.

We got our windows 7 and windows 10 laptops detecting the Tessel 2 and running blinkie.
Here's what we did:

1) Install LTS v4.4.4 from http://nodejs.org
2) Get the zip file https://gist.github.com/tcr/992978a5dbe5bff2e18f495c5c0973c3
3) Run node driver-clean.js
4) Plug in Tessel 2
5) Get Zadig - http://zadig.akeo.ie/ and run it, which should show the Tessel 2; simply click the install WinUSB button w/o changing anything - If you go to Device Manager, you should now notice that the three Tessel devices which previously had an ❗️ are now ok w/ the WinUSB driver
6) Run cmd as Administrator
7) Run npm i -g t2-cli
8) Run t2 list which should show the USB connection
9) Do the rest... http://tessel.github.io/t2-start/
10) Rejoice 

## Apply Ajax REST API call 

Based on [https://github.com/slamdata/purescript-halogen/blob/master/examples/effects-aff-ajax/src/Component.purs](https://github.com/slamdata/purescript-halogen/blob/master/examples/effects-aff-ajax/src/Component.purs)

