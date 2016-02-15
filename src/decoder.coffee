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

AMF3.UNDEFINED.decode = ->
	undefined

AMF3.NULL.decode = ->
	null

AMF3.FALSE.decode = ->
	false

AMF3.TRUE.decode = ->
	true

AMF3.INTEGER.decode = ->
	@readInt29()

AMF3.DOUBLE.decode = ->
	@readDoubleBE()

AMF3.STRING.decode = ->
	header = @readAMFHeader()
	return (@amf3StringReferences[header.value] || throw new Error("Invalid reference")) if not header.isDef
	return "" if header.value is 0
	str = @readByte(header.value, true).toString 'utf8'
	@amf3StringReferences.push str
	return str

AMF3.DATE.decode = ->
	header = @readAMFHeader()
	return (@amf3ObjectReferences[header.value] || throw new Error("Invalid reference")) if not header.isDef
	date = new Date @readDoubleBE()
	@amf3ObjectReferences.push date
	return date

AMF3.ARRAY.decode = ->
	header = @readAMFHeader()
	return (@amf3ObjectReferences[header.value] || throw new Error("Invalid reference")) if not header.isDef

	named = {}
	@amf3ObjectReferences.push(named)
	idx = @amf3ObjectReferences.length - 1

	while (key = @deserialize AMF3.STRING, AMF3) isnt ""
		named[key] = @decode AMF3

	if Object.keys(named).length > 0 # Associative array
		return named

	@amf3ObjectReferences.splice idx, 1

	ret = []
	@amf3ObjectReferences.push ret
	i = 0
	while i < header.value
		ret.push @decode AMF3
		i++ 

	return ret

readAMF3ObjectHeader = (flags) ->
	return (@amf3TraitReferences[flags >> 1] || throw new Error("Invalid reference")) if (flags & 1) is 0

	name = @deserialize AMF3.STRING, AMF3
	isExternalizable = (flags >> 1) & 1 is 1
	isDynamic = (flags >> 2) & 1 is 1
	staticKeyLen = flags >> 3
	trait = new classes.AMFTrait name, isDynamic, isExternalizable
	trait.staticFields.push @deserialize AMF3.STRING, AMF3 for x in [0..staticKeyLen - 1] if staticKeyLen isnt 0

	@amf3TraitReferences.push trait
	return trait

AMF3.OBJECT.decode = ->
	header = @readAMFHeader()
	return (@amf3ObjectReferences[header.value] || throw new Error("Invalid reference")) if not header.isDef

	trait = readAMF3ObjectHeader.call @, header.value
	if trait.externalizable
		if trait.name is "flex.messaging.io.ArrayCollection"
			array = @decode AMF3
			@amf3ObjectReferences.push array
			return array

		throw new Error "No externalizable registered with name #{trait.name}" if not AMFDecoder.amf3Externalizables[trait.name]

		object = AMFDecoder.amf3Externalizables[trait.name].read @
		@amf3ObjectReferences.push object
		return object

	ret = new classes.Serializable trait.name || undefined
	@amf3ObjectReferences.push ret
	ret[x] = @decode AMF3 for x in trait.staticFields

	if trait.dynamic
		while (key = @deserialize AMF3.STRING, AMF3) isnt ""
			ret[key] = @decode AMF3

	return ret

AMF3.BYTE_ARRAY.decode = ->
	header = @readAMFHeader()
	return (@amf3ObjectReferences[header.value] || throw new Error("Invalid reference")) if not header.isDef

	bytes = @readByte header.value
	@amf3ObjectReferences.push bytes
	return bytes

decodeVector = (func) ->
	header = @readAMFHeader()
	return (@amf3ObjectReferences[header.value] || throw new Error("Invalid reference")) if not header.isDef

	@readByte() # Fixed size byte
	res = (func.call @ for x in [0..header.value - 1])
	@amf3ObjectReferences.push res
	return res

AMF3.VECTOR_INT.decode = ->
	decodeVector.call @, ->
		return @readInt32BE()

AMF3.VECTOR_UINT.decode = ->
	decodeVector.call @, ->
		return @readInt32BE()

AMF3.VECTOR_DOUBLE.decode = ->
	decodeVector.call @, ->
		return @readDoubleBE()

AMF3.VECTOR_OBJECT.decode = ->
	decodeVector.call @, ->
		return @decode AMF3

AMF3.DICTIONARY.decode = ->
	header = @readAMFHeader()
	return (@amf3ObjectReferences[header.value] || throw new Error("Invalid reference")) if not header.isDef

	@readByte() # Don't care about this value
	ret = {}
	@amf3ObjectReferences.push ret
	ret[JSON.stringify(@decode AMF3)] = @decode AMF3 for x in [0..header.value - 1]
	return ret

module.exports = 
	AMFDecoder: AMFDecoder