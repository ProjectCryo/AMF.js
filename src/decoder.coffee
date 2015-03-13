types = require "./types"
classes = require "./classes"
reader = require "./reader"
AMF0 = types.AMF0
AMF3 = types.AMF3

class AMFDecoder extends reader.Reader
	@amf0Externalizables = {}
	@amf3Externalizables = {}

	###
	Registers a new externalizable. This function is required to be called
	by an externalizable because this function is used to figure out which
	methods to call when an object is read. Registering is not required if
	you are just encoding externalizables.
	###
	@register: (className, cls, amfType) ->
		amfType = amfType ? AMF3
		@amf0Externalizables[className] = cls if amfType is AMF0
		@amf3Externalizables[className] = cls if amfType is AMF3

	constructor: (readable) ->
		super readable

		@amf0References = []
		@amf3StringReferences = []
		@amf3ObjectReferences = []
		@amf3TraitReferences = []

	decode: (amfType) ->
		amfType = amfType ? AMF3
		type = @readByte()
		@deserialize type, amfType

	deserialize: (type, amfType) ->
		type = amfType.fromId type if type not instanceof types.AMFType
		type.decode.call @

AMF0.NUMBER.decode = ->
	@readDoubleBE()

AMF0.BOOLEAN.decode = ->
	@readByte() isnt 0x00

AMF0.STRING.decode = ->
	@readString()

AMF0.OBJECT.decode = ->
	ret = new classes.Serializable()
	while true
		name = @readString()
		type = @readByte()
		break if type is AMF0.OBJECT_END.id
		ret[name] = @deserialize type, AMF0
	@amf0References.push ret
	return ret

AMF0.NULL.decode = ->
	null

AMF0.UNDEFINED.decode = ->
	undefined

AMF0.REFERENCE.decode = ->
	@amf0References[@readUInt16BE()]

AMF0.ECMA_ARRAY.decode = ->
	@readInt32BE() # We don't actually do anything with the length, believe it or not
	ret = {}
	while true
		name = @readString()
		type = @readByte()
		break if type is AMF0.OBJECT_END.id
		ret[name] = @deserialize type, AMF0
	@amf0References.push ret
	return ret

AMF0.STRICT_ARRAY.decode = ->
	len = @readInt32BE()
	ret = (@decode AMF0 for x in [0..len - 1])
	@amf0References.push ret
	return ret

AMF0.DATE.decode = ->
	date = new Date @readDoubleBE()
	@readByte(2) # Timezone info we don't care about
	return date

AMF0.LONG_STRING.decode = ->
	@readByte(@readInt32BE()).toString "utf8"

AMF0.TYPED_OBJECT.decode = ->
	name = @readString()
	if AMFDecoder.amf0Externalizables[name]
		res = AMFDecoder.amf0Externalizables[name].read @
	else
		res = new classes.Serializable name
		while true
			name = @readString()
			type = @readByte()
			break if type is AMF0.OBJECT_END.id
			res[name] = @deserialize type, AMF0

	@amf0References.push res
	return res

AMF0.AMF3_OBJECT.decode = ->
	@decode AMF3

module.exports = 
	AMFDecoder: AMFDecoder