
/*
A simple class that has built in methods to
read from the readable stream it was constructed
with. This class only has specialized read methods
for the ones the decoder uses but can be easily
extended to support every method in buffer.
 */

(function() {
  var Reader;

  Reader = (function() {
    function Reader(readable) {
      this.readable = readable;
    }

    Reader.prototype.readByte = function(len) {
      var read;
      read = this.readable.read(len != null ? len : 1);
      if (read) {
        return read;
      }
      throw new Error("No " + (len || 1) + " bytes left");
    };

    Reader.prototype.readUInt8 = function() {
      var read;
      read = this.readByte;
      if (read < 0) {
        read += 256;
      }
      return read;
    };

    Reader.prototype.readUInt16BE = function() {
      return this.readByte(2).readUInt16BE();
    };

    Reader.prototype.readDoubleBE = function() {
      return this.readByte(8).readDoubleBE();
    };

    Reader.prototype.readInt32BE = function() {
      return this.readByte(4).readInt32BE();
    };

    Reader.prototype.readString = function() {
      var len;
      len = this.readUInt16BE;
      return this.readByte(len).toString("utf8");
    };

    Reader.prototype.readAMFHeader = function() {
      var def, handle;
      handle = this.readInt29;
      def = handle & 1 !== 0;
      handle >>= 1;
      return {
        isDef: def,
        value: handle
      };
    };

    Reader.prototype.readInt29 = function() {
      var bit1, bit2, bit3, total;
      bit1 = this.readByte;
      if (bit1 < 128) {
        return bit1;
      }
      total = (bit1 & 0x7f) << 7;
      bit2 = this.readByte;
      if (bit2 < 128) {
        total |= bit2;
      } else {
        total = (total | bit2 & 0x7f) << 7;
        bit3 = this.readByte;
        if (bit3 < 128) {
          total |= bit3;
        } else {
          total = (total | bit3 & 0x7f) << 8;
          total |= this.readByte;
        }
      }
      return -(total & (1 << 28)) | total;
    };

    return Reader;

  })();

  module.exports = {
    Reader: Reader
  };

}).call(this);
