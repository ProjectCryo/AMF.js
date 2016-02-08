classes = require "./classes"

class AMFType
	constructor: (@id, @name, referencable) ->
		@referencable = referencable ? false

	###
	Called when this type needs to encode a specific value.
	The result of the encoding is expected to be written to the
	writable string passed in. The context (the this value) is
	set to the encoder, so this.encode(value, type) can be used.
	###
	encode: (value, writable) ->
		throw new Error "No encoder defined for type #{@name}"

	###
	Called when this type needs to decode a specific value.
	The function is expected to read from the readable stream
	and to decode the value. The context (the this value) is set
	to the decoder, so this.decode(type) can be used.
	###
	decode: ->
		throw new Error "No decoder defined for type #{@name}"

module.exports =
	AMFType: AMFType
	AMF0:
		infer: (value) ->
			type = typeof value
			return module.exports.AMF0.NULL if value is null
			return module.exports.AMF0.UNDEFINED if type is "undefined"
			return value.type if value instanceof classes.ForcedTypeValue
			return module.exports.AMF0.NUMBER if type is "number"
			return module.exports.AMF0.BOOLEAN if type is "boolean"
			return module.exports.AMF0.LONG_STRING if type is "string" and value.length >= 0xFFFF
			return module.exports.AMF0.STRING if type is "string"
			return module.exports.AMF0.DATE if {}.toString.call(value) is "[object Date]" #Dirty hack for Date due to javascripts weird instanceof.
			return module.exports.AMF0.STRICT_ARRAY if value instanceof Array
			if value instanceof classes.Serializable
				return module.exports.AMF0.OBJECT if not value["__class"] or value["__class"] is ""
				return module.exports.AMF0.TYPED_OBJECT
			return module.exports.AMF0.ECMA_ARRAY if type is "object"

			throw new Error "Unable to infer AMF0 type of #{JSON.stringify(value)}"

		fromId: (id) ->
			return type for key, type of module.exports.AMF0 when type instanceof AMFType and type.id is id
			throw new Error "No AMF0 type with ID #{id}"

		NUMBER: new AMFType 0x00, "NUMBER"
		BOOLEAN: new AMFType 0x01, "BOOLEAN"
		STRING: new AMFType 0x02, "STRING"
		OBJECT: new AMFType 0x03, "OBJECT", true
		MOVIECLIP: new AMFType 0x04, "MOVIECLIP"
		NULL: new AMFType 0x05, "NULL"
		UNDEFINED: new AMFType 0x06, "UNDEFINED"
		REFERENCE: new AMFType 0x07, "REFERENCE"
		ECMA_ARRAY: new AMFType 0x08, "ECMA_ARRAY", true
		OBJECT_END: new AMFType 0x09, "OBJECT_END"
		STRICT_ARRAY: new AMFType 0x0a, "STRICT_ARRAY", true
		DATE: new AMFType 0x0b, "DATE"
		LONG_STRING: new AMFType 0x0c, "LONG_STRING"
		UNSUPPORTED: new AMFType 0x0d, "UNSUPPORTED"
		XML: new AMFType 0x0f, "XML"
		TYPED_OBJECT: new AMFType 0x10, "TYPED_OBJECT", true
		AMF3_OBJECT: new AMFType 0x11, "AMF3_OBJECT"
	AMF3:
		infer: (value) ->
			type = typeof value
			return module.exports.AMF3.NULL if value is null
			return module.exports.AMF3.UNDEFINED if type is "undefined"
			return value.type if value instanceof classes.ForcedTypeValue
			return module.exports.AMF3.FALSE if type is "boolean" and not value
			return module.exports.AMF3.TRUE if type is "boolean"
			return module.exports.AMF3.STRING if type is "string"
			return module.exports.AMF3.INTEGER if type is "number" and parseInt(value) is value
			return module.exports.AMF3.DOUBLE if type is "number"
			return module.exports.AMF3.DATE if {}.toString.call(value) is "[object Date]" #Dirty hack for Date due to javascripts weird instanceof.
			return module.exports.AMF3.BYTE_ARRAY if value instanceof Buffer
			return module.exports.AMF3.ARRAY if value instanceof Array
			return module.exports.AMF3.OBJECT if value instanceof classes.Serializable or value instanceof classes.Externalizable
			return module.exports.AMF3.ARRAY if type is "object" #Associative array

			throw new Error "Unable to infer AMF3 type of #{JSON.stringify(value)}"

		fromId: (id) ->
			return type for key, type of module.exports.AMF3 when type instanceof AMFType and type.id is id
			throw new Error "No AMF3 type with ID #{id}"

		UNDEFINED: new AMFType 0x00, "UNDEFINED"
		NULL: new AMFType 0x01, "NULL"
		FALSE: new AMFType 0x02, "FALSE"
		TRUE: new AMFType 0x03, "TRUE"
		INTEGER: new AMFType 0x04, "INTEGER"
		DOUBLE: new AMFType 0x05, "DOUBLE"
		STRING: new AMFType 0x06, "STRING", true
		XML_DOC: new AMFType 0x07, "XML_DOC", true
		DATE: new AMFType 0x08, "DATE", true
		ARRAY: new AMFType 0x09, "ARRAY", true
		OBJECT: new AMFType 0x0a, "OBJECT", true
		XML: new AMFType 0x0b, "XML", true
		BYTE_ARRAY: new AMFType 0x0c, "BYTE_ARRAY", true
		VECTOR_INT: new AMFType 0x0d, "VECTOR_INT", true
		VECTOR_UINT: new AMFType 0x0e, "VECTOR_UINT", true
		VECTOR_DOUBLE: new AMFType 0x0f, "VECTOR_DOUBLE", true
		VECTOR_OBJECT: new AMFType 0x10, "VECTOR_OBJECT", true
		DICTIONARY: new AMFType 0x11, "DICTIONARY", true
