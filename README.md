## AMF.js

As a simple AMF (Action Message Format) library for Node.js, AMF.js is capable of encoding and decoding most AMF0 and AMF3 types. Simply hook the AMFEncoder or AMFDecoder up to a stream and it will be able to read or write to/from the stream.

# Installation

    npm install [--save-dev] amfjs

# Usage
- __Encoding__
```
var amfjs = require("amfjs")
var encoder = new amfjs.AMFEncoder(someKindOfWritableStream)
encoder.writeObject(10, amfjs.AMF0) //Write as AMF0
encoder.writeObject(10, amfjs.AMF3) //Write as AMF3
```
- __Decoding__

```
var amfjs = require("amfjs")
var decoder = new amfjs.AMFDecoder(someKindOfReadableStream)
var value = decoder.decode(amfjs.AMF0) //Decode an AMF0 object
var value = decoder.decode(amfjs.AMF3) //Decode an AMF3 object
```
- __Encoding custom objects__  
Simply extend the Serializable class and call `writeObject` with an instance of your class. Any fields in the serializable not starting with `__` will automatically be encoded:
```
class MySerializable extends Serializable
  constructor: ->
    super "name.of.the.serializable"
```

- __Custom Externalizables__  
Simply extend the Externalizable class and register it in the decoder to allow for the custom encoding of objects. The `write` method will be called whenever an Externalizable needs to be encoded and the `this` value will be set to the encoder. The static `read` method will be called whenever an object with the Externizable's class is found. As with `write`, while invoking `read` the `this` value is set to the decoder.
```
class MyExternalizable extends Externalizable
  constructor: ->
    super "name.of.the.externalizable"
  write: ->
    //Do some writing stuff
    @write 0x10
  @read: ->
    //Do some reading stuff and return the read object
    @readByte()
    return new MyExternalizable()
    
//Register the externalizable
var amfjs = require("amfjs")
amfjs.AMFDecoder.register("name.of.the.externalizable", MyExternalizable, amfjs.AMF0) //Or AMF3  
```
    
