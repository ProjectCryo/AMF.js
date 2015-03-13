(function() {
  var AMF0, AMF3, AMFEncoder, classes, types, writer,
    extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    hasProp = {}.hasOwnProperty;

  types = require("./types");

  classes = require("./classes");

  writer = require("./writer");

  AMF0 = types.AMF0;

  AMF3 = types.AMF3;

  AMFEncoder = (function(superClass) {
    extend(AMFEncoder, superClass);


    /*
    	Creates a new AMFEncoder with a stream that it should
    	write to.
     */

    function AMFEncoder(writable) {
      AMFEncoder.__super__.constructor.call(this, writable);
      this.amf0References = [];
      this.amf3ObjectReferences = [];
      this.amf3StringReferences = [];
    }


    /*
    	Writes the specified object to the writable the encoder was
    	created with using the specified AMF version, of AMF3 by
    	default. This function appends the id of the value's AMF
    	type in front of the actual data.
     */

    AMFEncoder.prototype.write = function(value, amfType) {
      amfType = amfType != null ? amfType : AMF3;
      if (amfType === AMF3) {
        this.encodeAmf3(value);
      }
      if (amfType === AMF0) {
        return this.encodeAmf0(value);
      }
    };


    /*
    	Writes the specified object to the writable the encoder was
    	created with using the specified AMF version, or AMF3 by
    	default. This function differs from write in that this function
    	appends the 0x11 AMF3_OBJECT id if we are encoding for AMF3.
    	This method mainly exists for the actual encoding functions,
    	and should not be called by anyone just using the library.
    	This function appends the id of the value's AMF type in 
    	front of the actual data.
     */

    AMFEncoder.prototype.encode = function(value, amfType) {
      var typeOfObject;
      amfType = amfType != null ? amfType : AMF3;
      typeOfObject = amfType.infer(value);
      if (value instanceof classes.ForcedTypeValue) {
        value = value.value;
      }
      if (typeOfObject === AMF0.AMF3_OBJECT || amfType === AMF3) {
        this.write(AMF0.AMF3_OBJECT.id);
      }
      if (amfType === AMF3) {
        this.encodeAmf3(value, typeOfObject);
      }
      if (amfType === AMF0) {
        return this.encodeAmf0(value, typeOfObject);
      }
    };


    /*
    	Encodes the value as AMF0. This function should rarely be called
    	by the encoding code because better alternatives exist (write and encode).
     */

    AMFEncoder.prototype.encodeAmf0 = function(value, valueType) {
      valueType = valueType != null ? valueType : AMF0.infer(value);
      if (value instanceof classes.ForcedTypeValue) {
        value = value.value;
      }
      if (valueType.referencable) {
        if (this.amf0References.indexOf(value !== -1)) {
          this.write(AMF0.REFERENCE.id);
          this.writeUInt16BE(this.amf0References.indexOf(value), 1);
          return;
        } else {
          this.amf0References.push(value);
        }
      }
      this.write(valueType.id);
      return valueType.encode.call(this, value);
    };


    /*
    	Encodes the value as AMF3. This function should rarely be called
    	by the encoding code because better alternatives exist (write and encode).
     */

    AMFEncoder.prototype.encodeAmf3 = function(value, valueType) {
      var index;
      valueType = valueType != null ? valueType : AMF3.infer(value);
      if (value instanceof classes.ForcedTypeValue) {
        value = value.value;
      }
      if (valueType.referencable && value !== "") {
        if (valueType === AMF3.STRING) {
          index = this.amf3StringReferences.indexOf(value);
        }
        if (valueType !== AMF3.STRING) {
          index = this.amf3ObjectReferences.indexOf(value);
        }
        if (index !== -1) {
          this.write(valueType.id);
          this.serialize(index << 1, AMF3);
          return;
        } else {
          if (valueType === AMF3.STRING) {
            this.amf3StringReferences.push(value);
          }
          if (valueType !== AMF3.STRING) {
            this.amf3ObjectReferences.push(value);
          }
        }
      }
      this.write(valueType.id);
      return valueType.encode.call(this, value);
    };


    /*
    	Basically the same as encode and write, but does not write
    	the type identifier in front. This method should be used when
    	a single type is always expected (such as in the reference code)
    	above where it always expects an int29 with the reference index.
     */

    AMFEncoder.prototype.serialize = function(value, amfType) {
      var valueType;
      amfType = amfType != null ? amfType : AMF3;
      valueType = amfType.infer(value);
      if (value instanceof classes.ForcedTypeValue) {
        value = value.value;
      }
      return valueType.encode.call(this, value);
    };

    return AMFEncoder;

  })(writer.Writer);

  AMF0.NUMBER.encode = function(value) {
    return this.writeDoubleBE(value);
  };

  AMF0.BOOLEAN.encode = function(value) {
    if (value) {
      return this.write(0x01);
    } else {
      return this.write(0x00);
    }
  };

  AMF0.STRING.encode = function(value) {
    var str;
    str = new Buffer(value, "utf8");
    this.writeUInt16BE(str.length);
    return this.write(str);
  };

  AMF0.OBJECT.encode = function(value) {
    Object.keys(value).forEach((function(_this) {
      return function(key) {
        if (key.indexOf("__") === 0) {
          return;
        }
        _this.serialize(key, AMF0);
        return _this.encode(value[key], AMF0);
      };
    })(this));
    return this.write([0x00, 0x00, AMF0.OBJECT_END.id]);
  };

  AMF0.NULL.encode = function(value) {};

  AMF0.UNDEFINED.encode = function(value) {};

  AMF0.REFERENCE.encode = function(value) {
    return this.writeUInt16BE(value);
  };

  AMF0.ECMA_ARRAY.encode = function(value) {
    var keyLength;
    keyLength = Object.keys(value).length;
    this.write(keyLength >> 16);
    this.write(keyLength >> 8);
    this.write(keyLength >> 4);
    this.write(keyLength);
    Object.keys(value).forEach((function(_this) {
      return function(key) {
        if (key.indexOf("__") === 0) {
          return;
        }
        _this.serialize(key, AMF0);
        return _this.encode(value[key], AMF0);
      };
    })(this));
    return this.write([0x00, 0x00, AMF0.OBJECT_END.id]);
  };

  AMF0.OBJECT_END.encode = function(value) {
    throw new Error("Trying to write OBJECT_END?");
  };

  AMF0.STRICT_ARRAY.encode = function(value) {
    var i, len, val;
    this.write(value.length >> 16);
    this.write(value.length >> 8);
    this.write(value.length >> 4);
    this.write(value.length);
    for (i = 0, len = value.length; i < len; i++) {
      val = value[i];
      this.encode(val, AMF0);
    }
  };

  AMF0.DATE.encode = function(value) {
    this.writeDoubleBE(value.getTime());
    return this.write([0x00, 0x00]);
  };

  AMF0.LONG_STRING.encode = function(value) {
    var str;
    str = new Buffer(value, "utf8");
    this.writeInt32BE(str.length);
    return this.write(str);
  };

  AMF0.UNSUPPORTED.encode = function(value) {};

  AMF0.TYPED_OBJECT.encode = function(value) {
    if (value["__class"] && obj["__class"] !== "") {
      this.serialize(value["__class"], AMF0);
    }
    if (value instanceof classes.Externalizable) {
      return value.write(this);
    } else {
      Object.keys(value).forEach((function(_this) {
        return function(key) {
          if (key.indexOf("__") === 0) {
            return;
          }
          _this.serialize(key, AMF0);
          return _this.encode(value[key], AMF0);
        };
      })(this));
      return this.write([0x00, 0x00, AMF0.OBJECT_END.id]);
    }
  };

  AMF3.UNDEFINED.encode = AMF3.NULL.encode = AMF3.FALSE.encode = AMF3.TRUE.encode = function(value) {};

  AMF3.INTEGER.encode = function(value) {};

  AMF3.DOUBLE.encode = function(value) {
    return this.writeDoubleBE(value);
  };

  AMF3.STRING.encode = function(value) {
    var str;
    str = new Buffer(value, "utf8");
    this.serialize((str.length << 1) | 1, AMF3);
    return this.write(str);
  };

  AMF3.DATE.encode = function(value) {
    this.serialize(1, AMF3);
    return this.writeDoubleBE(value.getTime());
  };

  AMF3.ARRAY.encode = function(value) {
    var element, i, len, results;
    if (value instanceof Array) {
      this.write(value.length << 1 | 1);
      this.serialize("", AMF3);
      results = [];
      for (i = 0, len = value.length; i < len; i++) {
        element = value[i];
        results.push(this.encodeAmf3(element));
      }
      return results;
    } else {
      this.write(0x01);
      return Object.keys(value).forEach((function(_this) {
        return function(key) {
          if (key.indexOf("__") === 0) {
            return;
          }
          _this.serialize(key, AMF3);
          return _this.encodeAmf3(value[key]);
        };
      })(this));
    }
  };

  AMF3.OBJECT.encode = function(value) {
    var externalizable, header, key, keyCount, ref;
    if (!value["__class"] || value["__class"] === "") {
      this.write(0x0b);
      Object.keys(value).forEach((function(_this) {
        return function(key) {
          if (key.indexOf("__") === 0) {
            return;
          }
          _this.serialize(key, AMF3);
          return _this.encodeAmf3(value[key]);
        };
      })(this));
      return;
    }
    externalizable = value instanceof classes.Externalizable;
    for (key in value) {
      if (!hasProp.call(value, key)) continue;
      if (key.indexOf("__") !== 0) {
        keyCount = keyCount + 1 || 1;
      }
    }
    header = keyCount << 4 | (externalizable ? 1 : 0 << 2);
    header = (header | 2) | 1;
    this.write(header);
    this.serialize((ref = value["__class"]) != null ? ref : "", AMF3);
    if (externalizable) {
      return value.write(this);
    }
    Object.keys(value).forEach((function(_this) {
      return function(key) {
        if (key.indexOf("__") === 0) {
          return;
        }
        return _this.serialize(key, AMF3);
      };
    })(this));
    return Object.keys(value).forEach((function(_this) {
      return function(key) {
        if (key.indexOf("__") === 0) {
          return;
        }
        return _this.encodeAmf3(value[key]);
      };
    })(this));
  };

  AMF3.BYTE_ARRAY.encode = function(value) {
    this.serialize(value.length << 1 | 1, AMF3);
    return this.write(value);
  };

  module.exports = {
    AMFEncoder: AMFEncoder
  };

}).call(this);
