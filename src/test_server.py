import socket
import struct

def packet(message):
  Packet = struct.pack("!i",len(message))
  Packet += message
  return Packet

def unpacket(message):
  H,EventId,Code = struct.unpack("!iii",message[0:12])
  print "H = ",H,"EventId = ",EventId,",Code = ",Code
  if (H > 8):
    Res = message[12:]
    return Res
  else:  
    return Code 


def auth():
  Auth = "command:auth\nid:1\nversion:1.0.0\nusername:yeman\npassword:12306\n"  
  Packet = packet(Auth)
  return Packet 

def post():
  Post = "command:post\nid:2\nlength:9\nfilename:test.txt\ndirectory:video/subvideo\n"
  Packet = packet(Post)
  return Packet

def post_data(Indetify):
  Data = "command:postdata\nfiledescription:" + Indetify + "\nid:3\nbegin:0\nend:8\nvalue:liuguozhu\n" 
  Packet = packet(Data)
  return Packet

def main():
  sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  sock.connect(('localhost', 8001))

  # handle auth
  Auth = auth()
  sock.send(Auth)
  data = sock.recv(1024)
  Response = unpacket(data)
  print "auth response : " , Response

  # handle post
  Post = post()
  sock.send(Post)
  data = sock.recv(1024)
  Response = unpacket(data)
  #print repr(data)
  print "post response : " , Response

  # handle post data
  Post_Data = post_data(Response)
  sock.send(Post_Data)
  data = sock.recv(1024)
  Response = unpacket(data)
  print "post data response : " , Response
  

  sock.close()


if __name__ == '__main__':
  main() 
