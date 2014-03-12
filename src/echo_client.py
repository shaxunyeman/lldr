if __name__ == '__main__':
  import socket
  import struct
  sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
  sock.connect(('localhost', 8001))

  Content = 'liuguozhu,renliqiong,liuliu,rencaiping'
  Frm = '!i%ds' % len(Content)
  packet = struct.pack(Frm,len(Content),Content)
  sock.send(packet)

  data = sock.recv(1024)
  i,RecvData = struct.unpack(Frm,data)

  print 'echo << ' , RecvData

  sock.close()
