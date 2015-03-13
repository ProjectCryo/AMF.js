###
A simple class that will tell the encoder to encode a value as a
certain AMFType, regardless of its actual type or value.
###
class ForcedTypeValue
	constructor: (@value, @type) ->

###
Every Javascript object/CoffeeScript class should extend this
object if they want to serialize the object as a "named" object.
Passing a serializable object to the encoder will encode every
field, unless a getSerializableFields function is defined. This
function should return a list of strings of fields that should be
serialized. Note that by default every field starting with two underscores
(__) is ignored while serializing.
###
class Serializable
	constructor: (serializable_name) ->
		@__class = serializable_name

	###
	As noted above, a method as shown below can be added to
	prevent certain fields from being serialized. The same thing
	can also be achieved by adding two underscores in front of
	a variable name

	getSerializableFields: ->
		["fieldA", "fieldB", "fieldC"]
	###

###
Every class extending Externalizable indicates that it wants
to control how the object gets serialized and deserialized. Two
methods should be overridden to accomplish this effect: read and
write. Externalizables also have a name to link a received
extenalizable to their read function. Note that the read function
is static and is supposed to return a new instance of the class
it decodes.
###
class Externalizable
	constructor: (externalizable_name) ->
		@__class = externalizable_name

	###
	Called when this object needs to be written to a stream. This
	method is supposed to write the contents to the stream in such
	a way that read can decode it again. It should not return a
	value.
	###
	write: (encoder) ->
		throw new Error "Externalizable #{@__class} has no write function defined!"

	###
	Called when an Externalizable with the specified name is encountered.
	This function should read the contents of the readable and (using the
	decoder) create a new instance of this Externalizable with the read
	data. This method is expected to return the read value.
	###
	@read: (readable, decoder) ->
		throw new Error "Externalizable has no read function defined!"
	
module.exports =
	ForcedTypeValue: ForcedTypeValue
	Serializable: Serializable
	Externalizable: Externalizable
