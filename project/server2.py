import asyncio
import string
import aiohttp
import sys
import time

server_info={}
buffer=[]
def error(arg):
    print("? ", arg)
    sys.exit(0)

def messenger(client_id):
    message = 'AT ' + server_info[client_id]['last_serv'] + " " + server_info[client_id]['time_diff'] + " " + client_id + " " + server_info[client_id]['location'] + " " + server_info[client_id]['time_stamp'] + '\r\n'
    return message

    
async def fetch(session, url):
    async with session.get(url) as response:
        return await response.text()

async def called(location, radius, bound):
    async with aiohttp.ClientSession() as session:
        size =len(location)
        i=0
        while (i!=size):
            if ((location[i]=='+' or location[i]=='-') and i!=0):
                break
            i=i+1
        true_location = location[:i]+ "," + location[i+1:]
        info = await fetch(session, "https://maps.googleapis.com/maps/api/place/nearbysearch/json?location="+ true_location + "&radius=" + radius + "&key=AIzaSyD8h2eN8gP1G5wWc8FKc55sd93aEA8DrJI")
        i=0
        j=0
        lines = info.split("\n")
        length=len(lines)
        while((i+1)!=length):
            if ((lines[i]== "      },") and (lines[i+1]=="      {")):
                j=j+1
                print(j)
                if (j==bound):
                    break;
            i=i+1

        final = ""
        if (j==bound):
            final="\n".join(lines[:i])+"\n      }\n"
        else:
            final = info
        print(final)
        
        
class EchoServerClientProtocol(asyncio.Protocol):
    def __init__(self, serv_id, portnum):
        self.serv_id=serv_id
        self.portnum=portnum

    def connection_made(self, transport):
        self.transport = transport
        self.peername = transport.get_extra_info('peername') 
        print('Connection from {}'.format(self.peername))

    def data_received(self, data):
        message = data.decode()
        print('Data received: {!r}'.format(message))
        message_size = len(message)
        the_last=0
        if ((message[message_size-1]=="\n") or (message[message_size-1]=="\r") or (message[message_size-1]=="\r\n")):
            the_last=1
        
        mess_string="" #essentially, for handling
        i=0
        while(i!=message_size): #what is this? preliminary filtering: prepping the string for future splitting
            if ((message[i] = "\n") or (message[i]=="\r")):
                mess_strings+="\r\n"
            else:
                mess_strings+=message[i]
            i=i+1

        tempbuff=(mess_strings.split("\r\n") #exists here for filtering out strings
        temp_size = len(tempbuff) #new size here
        i=0
        while(i!=temp_size): # filter out all empty strings
            if (not((tempbuff[i]=="") or (tempbuff[i].isspace()))):
                buffer.append(tempbuff[i])
            i=i+1
        
                  
        buffer_len=len(buffer)
        while
        words=[]
        word=""
        i=0


        while (i!=size):
            if (message[i] in string.whitespace):
                if (word!= ""):
                    words.append(word)
                    word=""
                i=i+1
            else:
                word+=message[i]
                i=i+1

        if (words[0]=="IAMAT"):
            if (len(words)!=4): #arg lengh error
                error(message)
            if (len(words[2])==1):
                error(message)  #invalid location error
            i =(words[2])[1:].find("-")
            j=(words[2])[1:].find("+")
            if ((i==-1) and (j==-1)):
                error(message)
            if (i==-1):
                i==j
            try:
                x=float(words[2][:i+1])
                y=float(words[2][i+1:])
                z=float(words[3])
            except ValueError:
                error(message)

            return_message=""
            
            client_id=words[1]
            if (server_info.get(client_id, -1)!=-1): #if it does exist
                if (float(words[3])< float(server_info[client_id][time_stamp])):
                    return_message=messenger(client_id)

            server_info[client_id]={}
            server_info[client_id]['last_serv']= self.serv_id
            server_info[client_id]['location']= words[2]
            server_info[client_id]['time_stamp']=words[3]
            value = time.time()-float(words[3])
            sign=""
            if (value<0):
                sign="-"
            else:
                sign="+"
            server_info[client_id]['time_diff']=sign+str(value)
            return_message = messenger(client_id)    
            print('Send: {!r}'.format(return_message))
            self.transport.write(return_message.encode())

        elif(words[0]=="WHATSAT"):
            if (len(words)!=4): #arg lengh error                                          
                error(message)
            try:
                temp1=float(words[2])
                temp2=int(words[3])
                if (not((temp1<=50) and (temp1>=0) and (temp2>=0) and (temp2<=20))):
                    error(message)
            except ValueError:
                error(message)
            client_id=words[1]
            return_message=messenger(client_id)
            radius=words[2]
            bound =int(words[3])
            print('Send: {!r}'.format(return_message))
            self.transport.write(return_message.encode())
            asyncio.ensure_future(called(server_info[client_id]['location'],radius,bound))

        else:
            error(message)




            
    def connection_lost(self, exc):
        print('Lost connection of {}'.format(self.peername))
        self.transport.close()

        
def main():
    comm_network = {'Goloman': ['Hands','Holiday', 'Wilkes'],
                    'Hands': ['Goloman', 'Wilkes'],
                    'Holiday': ['Goloman','Welsh', 'Wilkes'],
                    'Welsh':['Holiday'],
                    'Wilkes':['Goloman', 'Hands', 'Holiday']
    }

    if (len(sys.argv) <2):
        error(sys.argv)
    serv_id = sys.argv[1]
    loop = asyncio.get_event_loop()
    if (serv_id=='Goloman'):
        coro = loop.create_server(lambda:EchoServerClientProtocol(serv_id, 12717), '0.0.0.0', 12717)
    elif (serv_id=='Hands'):
        coro = loop.create_server(lambda:EchoServerClientProtocol(serv_id, 12718), '0.0.0.0', 12718)
    elif (serv_id=='Holiday'):
        coro = loop.create_server(lambda:EchoServerClientProtocol(serv_id, 12719), '0.0.0.0', 12719)
    elif (serv_id=='Welsh'):
        coro = loop.create_server(lambda:EchoServerClientProtocol(serv_id, 12720), '0.0.0.0', 12720)
    elif (serv_id=='Wilkes'):
        coro = loop.create_server(lambda:EchoServerClientProtocol(serv_id, 12721), '0.0.0.0', 12721)
    else:
        print("? ", sys.argv)
        coro.connection_lost(exc)
    
    server = loop.run_until_complete(coro)
    loop = asyncio.get_event_loop()
    # Each client connection will create a new protocol instance
    # The IP address or hostname can be used.
    # 127.0.0.1 is intended for the 'localhost' loopback devices.
    # If you have multiple NIC(Network Interface Card)s, you may specify the specific IP address to be used (listen).
    # 0.0.0.0 is to use any available NIC device.
    # Serve requests until Ctrl+C is pressed
    print('Serving on {}'.format(server.sockets[0].getsockname()))
    try:
        loop.run_forever()
    except KeyboardInterrupt:
        pass

        
    # Close the server
    server.close()
    loop.run_until_complete(server.wait_closed())
    loop.close()

if __name__=="__main__":
    main()

