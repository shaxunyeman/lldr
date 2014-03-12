if __name__ == '__main__':
  import socket
  import struct
  sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  sock.connect(('localhost', 8001))

  packet = struct.pack('!iii8s10s6s',31,1,1,'1.0.0.0\n','liuguozhu\n','12306\n')
  print repr(packet)
  
  sock.send(packet)

  data = sock.recv(1024)
#  i,RecvData = struct.unpack(Frm,data)

#  print 'echo << ' , RecvData

  sock.close()
