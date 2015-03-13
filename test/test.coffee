assert = require 'assert'
should = require('chai').should()
assert = require('chai').assert
amf_js = require '../lib/amf-js.js'

AMF0 = amf_js.AMF0
AMF3 = amf_js.AMF3

Readable = require('stream').Readable
Writable = require('stream').Writable

# Create a simple readable stream
streamIn = new Readable()
streamIn._read = ->

# This writable stream writes to the readable stream
streamOut = new Writable()
streamOut._write = (chunk, len, cb) ->
	streamIn.push chunk
	cb()

# Create encoder and decoder
encoder = new amf_js.AMFEncoder streamOut
decoder = new amf_js.AMFDecoder streamIn

describe "amf.js - AMF0", ->
	it "should be able to serialize AMF0 numbers", ->
		encoder.writeObject 10, AMF0
		decoder.decode(AMF0).should.equal 10

		encoder.writeObject -10, AMF0
		decoder.decode(AMF0).should.equal -10

	it "should be able to serialize AMF0 booleans", ->
		encoder.writeObject true, AMF0
		decoder.decode(AMF0).should.equal true

		encoder.writeObject false, AMF0
		decoder.decode(AMF0).should.equal false

	it "should be able to serialize AMF0 strings", ->
		encoder.writeObject "Test123", AMF0
		decoder.decode(AMF0).should.equal "Test123"

	it "should be able to serialize null and undefined", ->
		encoder.writeObject null, AMF0
		assert.equal decoder.decode(AMF0), null

		encoder.writeObject undefined, AMF0
		assert.equal decoder.decode(AMF0), undefined

	it "should be able to serialize AMF0 simple arrays", ->
		encoder.writeObject [1, 2, 3], AMF0
		decoder.decode(AMF0).should.have.length 3

	it "should be able to serialize AMF0 dates", ->
		date = new Date()
		encoder.writeObject date, AMF0
		assert.equal date.getTime(), decoder.decode(AMF0).getTime()

	it "should be able to serialize AMF0 objects without names", ->
		theObject = new amf_js.Serializable ""
		theObject.number = 10

		encoder.writeObject theObject, AMF0
		read = decoder.decode(AMF0)

		read.should.be.an.instanceof amf_js.Serializable
		read.should.have.property('number').equal 10
		read.should.have.property('__class').equal undefined

	it "should be able to serialize AMF0 objects with names", ->
		theObject = new amf_js.Serializable "my.test.object"
		theObject.boolean = false

		encoder.writeObject theObject, AMF0
		read = decoder.decode(AMF0)

		read.should.be.an.instanceof amf_js.Serializable
		read.should.have.property('boolean').equal false
		read.should.have.property('__class').equal "my.test.object"

	it "should be able to serialize AMF0 maps that are not serializable instances", ->
		theMap = 
			value1: true
			value2: 10
			value3: "test"

		encoder.writeObject theMap, AMF0
		read = decoder.decode(AMF0)

		read.should.not.be.instanceof amf_js.Serializable
		read.should.have.property('value1').equal true
		read.should.have.property('value2').equal 10
		read.should.have.property('value3').equal "test"

	it "should be able to write and read custom AMF0 Externalizable instances", ->
		class TestExtern extends amf_js.Externalizable
			constructor: (@a, @b, @c) ->
				super "my.test.extern"
			write: (encoder) ->
				encoder.write [0x10, 0x11, 0x12]
			@read: (decoder) ->
				return new TestExtern decoder.readByte(), decoder.readByte(), decoder.readByte()

		amf_js.AMFDecoder.register "my.test.extern", TestExtern, AMF0
		amf_js.AMFDecoder.amf0Externalizables.should.have.property("my.test.extern").equal TestExtern

		encoder.writeObject new TestExtern(), AMF0
		read = decoder.decode(AMF0)
			
		read.should.be.an.instanceof TestExtern
		read.should.be.an.instanceof amf_js.Externalizable
		read.should.have.property('a').equal 0x10
		read.should.have.property('b').equal 0x11
		read.should.have.property('c').equal 0x12
		read.should.have.property('__class').equal "my.test.extern"

	it "should be able to serialize nested AMF0 types", ->
		theObject = new amf_js.Serializable "my.nested.test.object"
		theObject.a = 10
		theObject.b = 20.333333
		theObject.c = "Test"
		theObject.d = [1, 2, 3]
		theObject.e = new amf_js.Serializable "my.nested.empty.object"
		theObject.f = new amf_js.Serializable # Anon object

		encoder.writeObject theObject, AMF0
		read = decoder.decode(AMF0)

		read.should.be.an.instanceof amf_js.Serializable
		read.should.have.property('a').equal 10
		read.should.have.property('b').equal 20.333333
		read.should.have.property('c').equal "Test"
		read.should.have.deep.property('d.length').equal 3
		read.should.have.deep.property('e.__class').equal "my.nested.empty.object"
		read.should.have.deep.property('f.__class').equal undefined