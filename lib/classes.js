
/*
A simple class that will tell the encoder to encode a value as a
certain AMFType, regardless of its actual type or value.
 */

(function() {
  var AMFTrait, Externalizable, ForcedTypeValue, Serializable,
    extend = function(child, parent) { for (var key in parent) { if (hasProp.call(parent, key)) child[key] = parent[key]; } function ctor() { this.constructor = child; } ctor.prototype = parent.prototype; child.prototype = new ctor(); child.__super__ = parent.prototype; return child; },
    hasProp = {}.hasOwnProperty;

  ForcedTypeValue = (function() {
    function ForcedTypeValue(value, type) {
      this.value = value;
      this.type = type;
    }

    return ForcedTypeValue;

  })();


  /*
  A simple class storing all the settings for an AMF3 object trait.
   */

  AMFTrait = (function() {
    function AMFTrait(name, dynamic, externalizable) {
      this.name = name;
      this.dynamic = dynamic;
      this.externalizable = externalizable;
      this.staticFields = [];
    }

    return AMFTrait;

  })();


  /*
  Every Javascript object/CoffeeScript class should extend this
  object if they want to serialize the object as a "named" object.
  Passing a serializable object to the encoder will encode every
  field, unless a getSerializableFields function is defined. This
  function should return a list of strings of fields that should be
  serialized. Note that by default every field starting with two underscores
  (__) is ignored while serializing. Also note that the name is not
  required. If the name field is undefined or empty, it is assumed
  that the object is anonymous. The decoded object will then have no name
  either.
   */

  Serializable = (function() {
    function Serializable(serializable_name) {
      this.__class = serializable_name;
    }


    /*
    	As noted above, a method as shown below can be added to
    	prevent certain fields from being serialized. The same thing
    	can also be achieved by adding two underscores in front of
    	a variable name. Note that fields with two underscores are still
    	ignored if they are returned from this function.
    
    	getSerializableFields: ->
    		["fieldA", "fieldB", "fieldC"]
     */

    return Serializable;

  })();


  /*
  Every class extending Externalizable indicates that it wants
  to control how the object gets serialized and deserialized. Two
  methods should be overridden to accomplish this effect: read and
  write. Externalizables also have a name to link a received
  extenalizable to their read function. Note that the read function
  is static and is supposed to return a new instance of the class
  it decodes.
   */

  Externalizable = (function(superClass) {
    extend(Externalizable, superClass);

    function Externalizable(externalizable_name) {
      Externalizable.__super__.constructor.call(this, externalizable_name);
    }


    /*
    	Called when this object needs to be written to a stream. This
    	method is supposed to write the contents to the stream in such
    	a way that read can decode it again. It should not return a
    	value.
     */

    Externalizable.prototype.write = function(encoder) {
      throw new Error("Externalizable " + this.__class + " has no write function defined!");
    };


    /*
    	Called when an Externalizable with the specified name is encountered.
    	This function should read the contents of the readable and (using the
    	decoder) create a new instance of this Externalizable with the read
    	data. This method is expected to return the read value.
     */

    Externalizable.read = function(decoder) {
      throw new Error("Externalizable has no read function defined!");
    };

    return Externalizable;

  })(Serializable);

  module.exports = {
    ForcedTypeValue: ForcedTypeValue,
    AMFTrait: AMFTrait,
    Serializable: Serializable,
    Externalizable: Externalizable
  };

}).call(this);
