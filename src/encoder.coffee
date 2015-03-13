types = require "./types"
classes = require "./classes"
writer = require "./writer"
AMF0 = types.AMF0
AMF3 = types.AMF3

class AMFEncoder extends writer.Writer
	###
	Creates a new AMFEncoder with a stream that it should
	write to.
	###
	constructor: (writable) ->
		super writable
		# Setup reference tables so we can access them from the methods later.
		@amf0References = []
		@amf3ObjectReferences = []
		@amf3StringReferences = []

	###
	Writes the specified object to the writable the encoder was
	created with using the specified AMF version, of AMF3 by
	default. This function appends the id of the value's AMF
	type in front of the actual data.
	###
	write: (value, amfType) ->
		amfType = amfType ? AMF3 # We assume we are using AMF3 by default.
		
		this.encodeAmf3 value if amfType is AMF3
		this.encodeAmf0 value if amfType is AMF0

	###
	Writes the specified object to the writable the encoder was
	created with using the specified AMF version, or AMF3 by
	default. This function differs from write in that this function
	appends the 0x11 AMF3_OBJECT id if we are encoding for AMF3.
	This method mainly exists for the actual encoding functions,
	and should not be called by anyone just using the library.
	This function appends the id of the value's AMF type in 
	front of the actual data.
	###
	encode: (value, amfType) ->
		amfType = amfType ? AMF3 # We assume we are using AMF3 by default.
		typeOfObject = amfType.infer value
		value = value.value if value instanceof classes.ForcedTypeValue

		if typeOfObject is AMF0.AMF3_OBJECT or amfType is AMF3
			@write AMF0.AMF3_OBJECT.id

		this.encodeAmf3 value, typeOfObject if amfType is AMF3
		this.encodeAmf0 value, typeOfObject if amfType is AMF0

	###
	Encodes the value as AMF0. This function should rarely be called
	by the encoding code because better alternatives exist (write and encode).
	###
	encodeAmf0: (value, valueType) ->
		valueType = valueType ? AMF0.infer value
		value = value.value if value instanceof classes.ForcedTypeValue

		if valueType.referencable
			if @amf0References.indexOf value isnt -1
				@write AMF0.REFERENCE.id
				@writeUInt16BE @amf0References.indexOf(value), 1
				return
			else
				@amf0References.push value

		@write valueType.id
		valueType.encode.call this, value

	###
	Encodes the value as AMF3. This function should rarely be called
	by the encoding code because better alternatives exist (write and encode).
	###
	encodeAmf3: (value, valueType) ->
		valueType = valueType ? AMF3.infer value
		value = value.value if value instanceof classes.ForcedTypeValue

		if valueType.referencable and value isnt "" # We never reference empty strings
			index = @amf3StringReferences.indexOf value if valueType is AMF3.STRING 
			index = @amf3ObjectReferences.indexOf value if valueType isnt AMF3.STRING
			if index isnt -1
				@write valueType.id
				@serialize index << 1, AMF3 # We call this to make use of the int29 encoding we define later on
				return
			else
				@amf3StringReferences.push value if valueType is AMF3.STRING 
				@amf3ObjectReferences.push value if valueType isnt AMF3.STRING

		@write valueType.id
		valueType.encode.call this, value

	###
	Basically the same as encode and write, but does not write
	the type identifier in front. This method should be used when
	a single type is always expected (such as in the reference code)
	above where it always expects an int29 with the reference index.
	###
	serialize: (value, amfType) ->
		amfType = amfType ? AMF3
		valueType = amfType.infer value
		value = value.value if value instanceof classes.ForcedTypeValue
		valueType.encode.call this, value

AMF0.NUMBER.encode = (value) ->
	@writeDoubleBE value

AMF0.BOOLEAN.encode = (value) ->
	if value then @write 0x01 else @write 0x00

AMF0.STRING.encode = (value) ->
	str = new Buffer value, "utf8"
	@writeUInt16BE str.length
	@write str

AMF0.OBJECT.encode = (value) ->
	Object.keys(value).forEach (key) =>
		return if key.indexOf("__") == 0 # Ignore variables starting with __
		@serialize key, AMF0
		@encode value[key], AMF0
	@write [0x00, 0x00, AMF0.OBJECT_END.id] # Write empty string and then OBJECT_END to indicate end.

AMF0.NULL.encode = (value) ->
	return

AMF0.UNDEFINED.encode = (value) ->
	return

AMF0.REFERENCE.encode = (value) ->
	@writeUInt16BE value

AMF0.ECMA_ARRAY.encode = (value) ->
	# This is almost the same as OBJECT, but oh well
	keyLength = Object.keys(value).length
	@write keyLength >> 16
	@write keyLength >> 8
	@write keyLength >> 4
	@write keyLength

	Object.keys(value).forEach (key) =>
		return if key.indexOf("__") == 0 # Ignore variables starting with __
		@serialize key, AMF0
		@encode value[key], AMF0

	@write [0x00, 0x00, AMF0.OBJECT_END.id] # Write empty string and then OBJECT_END to indicate end.

AMF0.OBJECT_END.encode = (value) ->
	throw new Error "Trying to write OBJECT_END?"

AMF0.STRICT_ARRAY.encode = (value) ->
	@write value.length >> 16
	@write value.length >> 8
	@write value.length >> 4
	@write value.length 
	for val in value
		@encode val, AMF0
	return # This is not stricly neccessary, but prevents coffeescript from trying to turn the results of the previous loop into an array

AMF0.DATE.encode = (value) ->
	@writeDoubleBE value.getTime()
	@write [0x00, 0x00] #Timezone info we don't care about

AMF0.LONG_STRING.encode = (value) ->
	str = new Buffer value, "utf8"
	@writeInt32BE str.length
	@write str

AMF0.UNSUPPORTED.encode = (value) ->
	return

AMF0.TYPED_OBJECT.encode = (value) ->
	if value["__class"] and obj["__class"] isnt ""
		@serialize value["__class"], AMF0

	if value instanceof classes.Externalizable
		value.write @
	else
		Object.keys(value).forEach (key) =>
			return if key.indexOf("__") == 0 # Ignore variables starting with __
			@serialize key, AMF0
			@encode value[key], AMF0
		@write [0x00, 0x00, AMF0.OBJECT_END.id] # Empty string and 0x09

AMF3.UNDEFINED.encode = 
AMF3.NULL.encode =
AMF3.FALSE.encode = 
AMF3.TRUE.encode = (value) ->
	return # Undefined, null, false and true all use their id bit to show what they are and do not need more information.

AMF3.INTEGER.encode = (value) ->

AMF3.DOUBLE.encode = (value) ->
	@writeDoubleBE value

AMF3.STRING.encode = (value) ->
	str = new Buffer value, "utf8"
	@serialize (str.length << 1) | 1, AMF3
	@write str

AMF3.DATE.encode = (value) ->
	@serialize 1, AMF3 # This is the reference integer. We don't send cached dates, because we are lazy
	@writeDoubleBE value.getTime()

AMF3.ARRAY.encode = (value) ->
	if value instanceof Array
		@write value.length << 1 | 1
		@serialize "", AMF3 #Empty string to end the associative part
		@encodeAmf3 element for element in value
	else
		@write 0x01 # This 0x01 is 0 << 1 | 1, indicating a 0 length array.
		Object.keys(value).forEach (key) =>
			return if key.indexOf("__") == 0 # Ignore variables starting with __
			@serialize key, AMF3
			@encodeAmf3 value[key]


AMF3.OBJECT.encode = (value) ->
	if not value["__class"] or value["__class"] is ""
		@write 0x0b # 0x0b is 0 length, dynamic, non-externalizable type.
		Object.keys(value).forEach (key) =>
			return if key.indexOf("__") == 0 # Ignore variables starting with __
			@serialize key, AMF3
			@encodeAmf3 value[key]
		return
	
	externalizable = value instanceof classes.Externalizable
	keyCount = keyCount + 1 || 1 for own key of value when key.indexOf("__") isnt 0
	header = keyCount << 4 | if externalizable then 1 else 0 << 2
	header = ((header | 2) | 1)

	@write header
	@serialize value["__class"] ? "", AMF3
	return value.write @ if externalizable
	Object.keys(value).forEach (key) =>
		return if key.indexOf("__") == 0 # Ignore variables starting with __
		@serialize key, AMF3
	Object.keys(value).forEach (key) =>
		return if key.indexOf("__") == 0 # Ignore variables starting with __
		@encodeAmf3 value[key]

AMF3.BYTE_ARRAY.encode = (value) ->
	@serialize value.length << 1 | 1, AMF3
	@write value

module.exports = 
	AMFEncoder: AMFEncoder