###
A simple class that has built in methods to
write to the writable stream it was constructed
with. This class only has specialized write methods
for the ones the encoder is used but can be
easily extended to support every method in buffer.
###
class Writer
	constructor: (@writable) ->
	
	###
	Writes the specified value to the stream. This method is
	capable of writing byte arrays, a single byte, a string and
	buffers.
	###
	write: (value) ->
		@writable.write new Buffer value if value instanceof Array
		@writable.write new Buffer [value] if typeof value is "number"
		@writable.write new Buffer value, "utf8" if typeof value is "string"
		@writable.write value if value instanceof Buffer
		throw new Error "Do not know how to write #{JSON.stringify(value)}!" 

	writeUInt16BE: (value) ->
		buf = new Buffer 2
		buf.writeUInt16BE value
		@write buf

	writeUInt32BE: (value) ->
		buf = new Buffer 4
		buf.writeUInt32BE value
		@write buf

	writeInt16BE: (value) ->
		buf = new Buffer 2
		buf.writeInt16BE value
		@write buf

	writeInt32BE: (value) ->
		buf = new Buffer 4
		buf.writeInt32BE value
		@write buf

	writeDoubleBE: (value) ->
		buf = new Buffer 8
		buf.writeDoubleBE value
		@write buf

module.exports = 
	Writer: Writer