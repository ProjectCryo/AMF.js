(function() {
  var AMF0, AMF3, AMFDecoder, classes, decodeVector, readAMF3ObjectHeader, reader, types,
    extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    hasProp = {}.hasOwnProperty;

  types = require("./types");

  classes = require("./classes");

  reader = require("./reader");

  AMF0 = types.AMF0;

  AMF3 = types.AMF3;

  AMFDecoder = (function(superClass) {
    extend(AMFDecoder, superClass);

    AMFDecoder.amf0Externalizables = {};

    AMFDecoder.amf3Externalizables = {};


    /*
    	Registers a new externalizable. This function is required to be called
    	by an externalizable because this function is used to figure out which
    	methods to call when an object is read. Registering is not required if
    	you are just encoding externalizables.
     */

    AMFDecoder.register = function(className, cls, amfType) {
      amfType = amfType != null ? amfType : AMF3;
      if (amfType === AMF0) {
        this.amf0Externalizables[className] = cls;
      }
      if (amfType === AMF3) {
        return this.amf3Externalizables[className] = cls;
      }
    };

    function AMFDecoder(readable) {
      AMFDecoder.__super__.constructor.call(this, readable);
      this.amf0References = [];
      this.amf3StringReferences = [];
      this.amf3ObjectReferences = [];
      this.amf3TraitReferences = [];
    }

    AMFDecoder.prototype.decode = function(amfType) {
      var type;
      amfType = amfType != null ? amfType : AMF3;
      type = this.readByte();
      return this.deserialize(type, amfType);
    };

    AMFDecoder.prototype.deserialize = function(type, amfType) {
      if (!(type instanceof types.AMFType)) {
        type = amfType.fromId(type);
      }
      return type.decode.call(this);
    };

    return AMFDecoder;

  })(reader.Reader);

  AMF0.NUMBER.decode = function() {
    return this.readDoubleBE();
  };

  AMF0.BOOLEAN.decode = function() {
    return this.readByte() !== 0x00;
  };

  AMF0.STRING.decode = function() {
    return this.readString();
  };

  AMF0.OBJECT.decode = function() {
    var name, ret, type;
    ret = new classes.Serializable();
    while (true) {
      name = this.readString();
      type = this.readByte();
      if (type === AMF0.OBJECT_END.id) {
        break;
      }
      ret[name] = this.deserialize(type, AMF0);
    }
    this.amf0References.push(ret);
    return ret;
  };

  AMF0.NULL.decode = function() {
    return null;
  };

  AMF0.UNDEFINED.decode = function() {
    return void 0;
  };

  AMF0.REFERENCE.decode = function() {
    return this.amf0References[this.readUInt16BE()];
  };

  AMF0.ECMA_ARRAY.decode = function() {
    var name, ret, type;
    this.readInt32BE();
    ret = {};
    while (true) {
      name = this.readString();
      type = this.readByte();
      if (type === AMF0.OBJECT_END.id) {
        break;
      }
      ret[name] = this.deserialize(type, AMF0);
    }
    this.amf0References.push(ret);
    return ret;
  };

  AMF0.STRICT_ARRAY.decode = function() {
    var len, ret, x;
    len = this.readInt32BE();
    ret = (function() {
      var j, ref, results;
      results = [];
      for (x = j = 0, ref = len - 1; 0 <= ref ? j <= ref : j >= ref; x = 0 <= ref ? ++j : --j) {
        results.push(this.decode(AMF0));
      }
      return results;
    }).call(this);
    this.amf0References.push(ret);
    return ret;
  };

  AMF0.DATE.decode = function() {
    var date;
    date = new Date(this.readDoubleBE());
    this.readByte(2);
    return date;
  };

  AMF0.LONG_STRING.decode = function() {
    return this.readByte(this.readInt32BE()).toString("utf8");
  };

  AMF0.TYPED_OBJECT.decode = function() {
    var name, res, type;
    name = this.readString();
    if (AMFDecoder.amf0Externalizables[name]) {
      res = AMFDecoder.amf0Externalizables[name].read(this);
    } else {
      res = new classes.Serializable(name);
      while (true) {
        name = this.readString();
        type = this.readByte();
        if (type === AMF0.OBJECT_END.id) {
          break;
        }
        res[name] = this.deserialize(type, AMF0);
      }
    }
    this.amf0References.push(res);
    return res;
  };

  AMF0.AMF3_OBJECT.decode = function() {
    return this.decode(AMF3);
  };

  AMF3.UNDEFINED.decode = function() {
    return void 0;
  };

  AMF3.NULL.decode = function() {
    return null;
  };

  AMF3.FALSE.decode = function() {
    return false;
  };

  AMF3.TRUE.decode = function() {
    return true;
  };

  AMF3.INTEGER.decode = function() {
    return this.readInt29();
  };

  AMF3.DOUBLE.decode = function() {
    return this.readDoubleBE();
  };

  AMF3.STRING.decode = function() {
    var header, str;
    header = this.readAMFHeader();
    if (!header.isDef) {
      return this.amf3StringReferences[header.value];
    }
    if (header.value === 0) {
      return "";
    }
    str = this.readByte(header.value, true).toString('utf8');
    this.amf3StringReferences.push(str);
    return str;
  };

  AMF3.DATE.decode = function() {
    var date, header;
    header = this.readAMFHeader();
    if (!header.isDef) {
      return this.amf3ObjectReferences[header.value];
    }
    date = new Date(this.readDoubleBE());
    this.amf3ObjectReferences.push(date);
    return date;
  };

  AMF3.ARRAY.decode = function() {
    var header, i, key, named, ret;
    header = this.readAMFHeader();
    if (!header.isDef) {
      return this.amf3ObjectReferences[header.value];
    }
    named = {};
    while ((key = this.deserialize(AMF3.STRING, AMF3)) !== "") {
      named[key] = this.decode(AMF3);
    }
    if (Object.keys(named).length > 0) {
      this.amf3ObjectReferences.push(named);
      return named;
    }
    ret = [];
    i = 0;
    while (i < header.value) {
      ret.push(this.decode(AMF3));
      i++;
    }
    this.amf3ObjectReferences.push(ret);
    return ret;
  };

  readAMF3ObjectHeader = function(flags) {
    var isDynamic, isExternalizable, j, name, ref, staticKeyLen, trait, x;
    if ((flags & 1) === 0) {
      return this.amf3TraitReferences[flags >> 1];
    }
    name = this.deserialize(AMF3.STRING, AMF3);
    isExternalizable = (flags >> 1) & 1 === 1;
    isDynamic = (flags >> 2) & 1 === 1;
    staticKeyLen = flags >> 3;
    trait = new classes.AMFTrait(name, isDynamic, isExternalizable);
    if (staticKeyLen !== 0) {
      for (x = j = 0, ref = staticKeyLen - 1; 0 <= ref ? j <= ref : j >= ref; x = 0 <= ref ? ++j : --j) {
        trait.staticFields.push(this.deserialize(AMF3.STRING, AMF3));
      }
    }
    this.amf3TraitReferences.push(trait);
    return trait;
  };

  AMF3.OBJECT.decode = function() {
    var header, j, key, len1, ref, ret, trait, x;
    header = this.readAMFHeader();
    if (!header.isDef) {
      return this.amf3ObjectReferences[header.value];
    }
    trait = readAMF3ObjectHeader.call(this, header.value);
    if (trait.externalizable) {
      if (!AMFDecoder.amf3Externalizables[trait.name]) {
        throw new Error("No externalizable registered with name " + trait.name);
      }
      return AMFDecoder.amf3Externalizables[trait.name].read(this);
    }
    ret = new classes.Serializable(trait.name || void 0);
    ref = trait.staticFields;
    for (j = 0, len1 = ref.length; j < len1; j++) {
      x = ref[j];
      ret[x] = this.decode(AMF3);
    }
    if (trait.dynamic) {
      while ((key = this.deserialize(AMF3.STRING, AMF3)) !== "") {
        ret[key] = this.decode(AMF3);
      }
    }
    return ret;
  };

  AMF3.BYTE_ARRAY.decode = function() {
    var bytes, header;
    header = this.readAMFHeader();
    if (!header.isDef) {
      return this.amf3ObjectReferences[header.value];
    }
    bytes = this.readByte(header.value);
    this.amf3ObjectReferences.push(bytes);
    return bytes;
  };

  decodeVector = function(func) {
    var header, res, x;
    header = this.readAMFHeader();
    if (!header.isDef) {
      return this.amf3ObjectReferences[header.value];
    }
    this.readByte();
    res = (function() {
      var j, ref, results;
      results = [];
      for (x = j = 0, ref = header.value - 1; 0 <= ref ? j <= ref : j >= ref; x = 0 <= ref ? ++j : --j) {
        results.push(func.call(this));
      }
      return results;
    }).call(this);
    this.amf3ObjectReferences.push(res);
    return res;
  };

  AMF3.VECTOR_INT.decode = function() {
    return decodeVector.call(this, function() {
      return this.readInt32BE();
    });
  };

  AMF3.VECTOR_UINT.decode = function() {
    return decodeVector.call(this, function() {
      return this.readInt32BE();
    });
  };

  AMF3.VECTOR_DOUBLE.decode = function() {
    return decodeVector.call(this, function() {
      return this.readDoubleBE();
    });
  };

  AMF3.VECTOR_OBJECT.decode = function() {
    return decodeVector.call(this, function() {
      return this.decode(AMF3);
    });
  };

  AMF3.DICTIONARY.decode = function() {
    var header, j, ref, ret, x;
    header = this.readAMFHeader();
    if (!header.isDef) {
      return this.amf3ObjectReferences[header.value];
    }
    this.readByte();
    ret = {};
    for (x = j = 0, ref = header.value - 1; 0 <= ref ? j <= ref : j >= ref; x = 0 <= ref ? ++j : --j) {
      ret[JSON.stringify(this.decode(AMF3))] = this.decode(AMF3);
    }
    return ret;
  };

  module.exports = {
    AMFDecoder: AMFDecoder
  };

}).call(this);
