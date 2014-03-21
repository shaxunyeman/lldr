import struct

if __name__ == '__main__':
  # pack
  Auth = "command:auth\nid:1\nversion:1.0.0\nusername:yeman\npassword:12306\n"  

  Message = struct.pack("!i",len(Auth))
  size = struct.calcsize("!i")
  print "size = " , size 
  Message += Auth

  #unpack
  H = struct.unpack("!i",Message[0:size])
  Str = Message[4:H[0]]

  print "Head = ", H[0], "Str = ",Str


  

