(function() {
  var AMF0, AMF3, AMFDecoder, classes, reader, types,
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
      var i, ref, results;
      results = [];
      for (x = i = 0, ref = len - 1; 0 <= ref ? i <= ref : i >= ref; x = 0 <= ref ? ++i : --i) {
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

  module.exports = {
    AMFDecoder: AMFDecoder
  };

}).call(this);
