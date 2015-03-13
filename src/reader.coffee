###
A simple class that has built in methods to
read from the readable stream it was constructed
with. This class only has specialized read methods
for the ones the decoder uses but can be easily
extended to support every method in buffer.
###
class Reader
	constructor: (@readable) ->
	
	readByte: (len, alwaysReturnBuffer) ->
		read = @readable.read len ? 1
		return (if read.length is 1 and not alwaysReturnBuffer then read[0] else read) if read
		throw new Error "No #{len || 1} bytes left"

	readUInt8: ->
		read = @readByte()
		read += 256 if read < 0
		return read

	readUInt16BE: ->
		@readByte(2).readUInt16BE()

	readDoubleBE: ->
		@readByte(8).readDoubleBE()

	readInt32BE: ->
		@readByte(4).readInt32BE()

	readString: ->
		len = @readUInt16BE()
		return "" if len is 0
		@readByte(len, true).toString "utf8"

	readAMFHeader: ->
		handle = @readInt29()
		def = handle & 1 isnt 0
		handle >>= 1

		isDef: def
		value: handle

	readInt29: ->
		bit1 = @readByte()
		return bit1 if bit1 < 128
		total = (bit1 & 0x7f) << 7

		bit2 = @readByte()
		if bit2 < 128
			total |= bit2
		else
			total = (total | bit2 & 0x7f) << 7
			bit3 = @readByte()
			if bit3 < 128
				total |= bit3
			else
				total = (total | bit3 & 0x7f) << 8
				total |= @readByte()

		-(total & (1 << 28)) | total

module.exports = 
	Reader: Reader