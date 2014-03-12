import struct

if __name__ == '__main__':
  Name = 'liuguozhu'
  FrmName = '%ds' % len(Name)
  Pwd = "12306"
  FrmPwd = '%ds' % len(Pwd)
  Frm = FrmName + FrmPwd
  packet = struct.pack(Frm,Name,Pwd)
#print repr(packet)
#print struct.calcsize(Frm)

  DataFrm = '!%ds' % struct.calcsize(Frm)
  Data = struct.pack(DataFrm,repr(packet))
  print repr(Data)
