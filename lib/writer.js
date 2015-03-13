
/*
A simple class that has built in methods to
write to the writable stream it was constructed
with. This class only has specialized write methods
for the ones the encoder is used but can be
easily extended to support every method in buffer.
 */

(function() {
  var Writer;

  Writer = (function() {
    function Writer(writable) {
      this.writable = writable;
    }


    /*
    	Writes the specified value to the stream. This method is
    	capable of writing byte arrays, a single byte, a string and
    	buffers.
     */

    Writer.prototype.write = function(value) {
      if (value instanceof Array) {
        return this.writable.write(new Buffer(value));
      }
      if (typeof value === "number") {
        return this.writable.write(new Buffer([value]));
      }
      if (typeof value === "string") {
        return this.writable.write(new Buffer(value, "utf8"));
      }
      if (value instanceof Buffer) {
        return this.writable.write(value);
      }
      throw new Error("Do not know how to write " + (JSON.stringify(value)) + "!");
    };

    Writer.prototype.writeUInt16BE = function(value) {
      var buf;
      buf = new Buffer(2);
      buf.writeUInt16BE(value);
      return this.write(buf);
    };

    Writer.prototype.writeUInt32BE = function(value) {
      var buf;
      buf = new Buffer(4);
      buf.writeUInt32BE(value);
      return this.write(buf);
    };

    Writer.prototype.writeInt16BE = function(value) {
      var buf;
      buf = new Buffer(2);
      buf.writeInt16BE(value);
      return this.write(buf);
    };

    Writer.prototype.writeInt32BE = function(value) {
      var buf;
      buf = new Buffer(4);
      buf.writeInt32BE(value);
      return this.write(buf);
    };

    Writer.prototype.writeDoubleBE = function(value) {
      var buf;
      buf = new Buffer(8);
      buf.writeDoubleBE(value);
      return this.write(buf);
    };

    return Writer;

  })();

  module.exports = {
    Writer: Writer
  };

}).call(this);
