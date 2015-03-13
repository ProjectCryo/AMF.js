(function() {
  var AMFType, classes;

  classes = require("./classes");

  AMFType = (function() {
    function AMFType(id1, name, referencable) {
      this.id = id1;
      this.name = name;
      this.referencable = referencable != null ? referencable : false;
    }


    /*
    	Called when this type needs to encode a specific value.
    	The result of the encoding is expected to be written to the
    	writable string passed in. The context (the this value) is
    	set to the encoder, so this.encode(value, type) can be used.
     */

    AMFType.prototype.encode = function(value, writable) {
      throw new Error("No encoder defined for type " + this.name);
    };


    /*
    	Called when this type needs to decode a specific value.
    	The function is expected to read from the readable stream
    	and to decode the value. The context (the this value) is set
    	to the decoder, so this.decode(type) can be used.
     */

    AMFType.prototype.decode = function() {
      throw new Error("No decoder defined for type " + this.name);
    };

    return AMFType;

  })();

  module.exports = {
    AMFType: AMFType,
    AMF0: {
      infer: function(value) {
        var type;
        type = typeof value;
        if (value === null) {
          return module.exports.AMF0.NULL;
        }
        if (type === "undefined") {
          return module.exports.AMF0.UNDEFINED;
        }
        if (value instanceof classes.ForcedTypeValue) {
          return value.type;
        }
        if (type === "number") {
          return module.exports.AMF0.NUMBER;
        }
        if (type === "boolean") {
          return module.exports.AMF0.BOOLEAN;
        }
        if (type === "string" && value.length >= 0xFFFF) {
          return module.exports.AMF0.LONG_STRING;
        }
        if (type === "string") {
          return module.exports.AMF0.STRING;
        }
        if ({}.toString.call(value) === "[object Date]") {
          return module.exports.AMF0.DATE;
        }
        if (value instanceof Array) {
          return module.exports.AMF0.STRICT_ARRAY;
        }
        if (value instanceof classes.Serializable) {
          if (!value["__class"] || value["__class"] === "") {
            return module.exports.AMF0.OBJECT;
          }
          return module.exports.AMF0.TYPED_OBJECT;
        }
        if (type === "object") {
          return module.exports.AMF0.ECMA_ARRAY;
        }
        throw new Error("Unable to infer AMF0 type of " + (JSON.stringify(value)));
      },
      fromId: function(id) {
        var key, ref, type;
        ref = module.exports.AMF0;
        for (key in ref) {
          type = ref[key];
          if (type instanceof AMFType && type.id === id) {
            return type;
          }
        }
        throw new Error("No AMF0 type with ID " + id);
      },
      NUMBER: new AMFType(0x00, "NUMBER"),
      BOOLEAN: new AMFType(0x01, "BOOLEAN"),
      STRING: new AMFType(0x03, "STRING"),
      OBJECT: new AMFType(0x04, "OBJECT", true),
      MOVIECLIP: new AMFType(0x05, "MOVIECLIP"),
      NULL: new AMFType(0x06, "NULL"),
      UNDEFINED: new AMFType(0x07, "UNDEFINED"),
      REFERENCE: new AMFType(0x08, "REFERENCE"),
      ECMA_ARRAY: new AMFType(0x09, "ECMA_ARRAY", true),
      OBJECT_END: new AMFType(0x0a, "OBJECT_END"),
      STRICT_ARRAY: new AMFType(0x0b, "STRICT_ARRAY", true),
      DATE: new AMFType(0x0c, "DATE"),
      LONG_STRING: new AMFType(0x0d, "LONG_STRING"),
      UNSUPPORTED: new AMFType(0x0e, "UNSUPPORTED"),
      XML: new AMFType(0x0f, "XML"),
      TYPED_OBJECT: new AMFType(0x10, "TYPED_OBJECT", true),
      AMF3_OBJECT: new AMFType(0x11, "AMF3_OBJECT")
    },
    AMF3: {
      infer: function(value) {
        var type;
        type = typeof value;
        if (value === null) {
          return module.exports.AMF3.NULL;
        }
        if (type === "undefined") {
          return module.exports.AMF3.UNDEFINED;
        }
        if (value instanceof classes.ForcedTypeValue) {
          return value.type;
        }
        if (type === "boolean" && !value) {
          return module.exports.AMF3.FALSE;
        }
        if (type === "boolean") {
          return module.exports.AMF3.TRUE;
        }
        if (type === "string") {
          return module.exports.AMF3.STRING;
        }
        if (type === "number" && parseInt(value) === value) {
          return module.exports.AMF3.INTEGER;
        }
        if (type === "number") {
          return module.exports.AMF3.DOUBLE;
        }
        if ({}.toString.call(value === "[object Date]")) {
          return module.exports.AMF3.DATE;
        }
        if (value instanceof Buffer) {
          return module.exports.AMF3.BYTE_ARRAY;
        }
        if (value instanceof Array) {
          return module.exports.AMF3.ARRAY;
        }
        if (value instanceof classes.Serializable || value instanceof classes.Externalizable) {
          return module.exports.AMF3.OBJECT;
        }
        if (type === "object") {
          return module.exports.AMF3.ARRAY;
        }
        throw new Error("Unable to infer AMF3 type of " + (JSON.stringify(value)));
      },
      fromId: function(id) {
        var key, ref, type;
        ref = module.exports.AMF3;
        for (key in ref) {
          type = ref[key];
          if (type instanceof AMFType && type.id === id) {
            return type;
          }
        }
        throw new Error("No AMF3 type with ID " + id);
      },
      UNDEFINED: new AMFType(0x00, "UNDEFINED"),
      NULL: new AMFType(0x01, "NULL"),
      FALSE: new AMFType(0x02, "FALSE"),
      TRUE: new AMFType(0x03, "TRUE"),
      INTEGER: new AMFType(0x04, "INTEGER"),
      DOUBLE: new AMFType(0x05, "DOUBLE"),
      STRING: new AMFType(0x06, "STRING", true),
      XML_DOC: new AMFType(0x07, "XML_DOC", true),
      DATE: new AMFType(0x08, "DATE", true),
      ARRAY: new AMFType(0x09, "ARRAY", true),
      OBJECT: new AMFType(0x0a, "OBJECT", true),
      XML: new AMFType(0x0b, "XML", true),
      BYTE_ARRAY: new AMFType(0x0c, "BYTE_ARRAY", true),
      VECTOR_INT: new AMFType(0x0d, "VECTOR_INT", true),
      VECTOR_UINT: new AMFType(0x0e, "VECTOR_UINT", true),
      VECTOR_DOUBLE: new AMFType(0x0f, "VECTOR_DOUBLE", true),
      VECTOR_OBJECT: new AMFType(0x10, "VECTOR_OBJECT", true),
      DICTIONARY: new AMFType(0x11, "DICTIONARY", true)
    }
  };

}).call(this);
