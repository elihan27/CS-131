import asyncio
import string
import aiohttp
import sys
import time
import logging

#As of 9:30 AM, 06/08/2018:
#has implemented "WHATSAT", "IAMAT", and "AT", dictionary, servers talking to each other
#need to implement:  logger, buffer


server_info={}
#buffer=[]
portnumbers= {
    'Goloman': 12717,
    'Hands': 12718,
    'Holiday': 12719,
    'Welsh': 12720,
    'Wilkes': 12721
}

logs={
    'Goloman': 'Goloman.log',
    'Hands': 'Hands.log',
    'Holiday': 'Holiday.log',
    'Welsh': 'Welsh.log',
    'Wilkes': 'Wilkes.log'
}

comm_network = {'Goloman': ['Hands','Holiday', 'Wilkes'],
                'Hands': ['Goloman', 'Wilkes'],
                'Holiday': ['Goloman','Welsh', 'Wilkes'],
                'Welsh':['Holiday'],
                'Wilkes':['Goloman', 'Hands', 'Holiday']
}
def error(arg):
    print("? ", arg)
    sys.exit(0)

def messenger(client_id):
    message = 'AT ' + server_info[client_id]['last_serv'] + " " + server_info[client_id]['time_diff'] + " " + client_id + " " + server_info[client_id]['location'] + " " + server_info[client_id]['time_stamp'] + '\r\n'
    logging.basicConfig(filename=logs[server_info[client_id]['last_serv']],level=logging.INFO)
    logging.info(message)
    return message

def update_server(client_id, server, location, time_stamp):
    server_info[client_id]={}
    server_info[client_id]['last_serv']= server
    server_info[client_id]['location']= location
    server_info[client_id]['time_stamp']=time_stamp
    value = time.time()-float(time_stamp)
    sign=""
    if (value>0):
        sign="+"
    server_info[client_id]['time_diff']=sign+str(value)

async def send_data(loop, serv_id, message):
    for server in comm_network[serv_id]:
        try:
            coro = await (loop).create_connection(lambda:ClientProtocol(message,loop), '0.0.0.0', portnumbers[server])
            logging.basicConfig(filename=logs[serv_id],level=logging.INFO)
            logging.info("Opened connection to " +server)
            logging.info("Sent: "+ message)
            logging.info("Closed connection to " +server)
        except ConnectionRefusedError:
            logging.basicConfig(filename=logs[serv_id],level=logging.INFO)
            logging.info("Could not connect to " +server)
        
async def fetch(session, url):
    async with session.get(url) as response:
        return await response.text()

async def called(location, radius, bound, proto):
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
        #        print(j)
                if (j==bound):
                    break;
            i=i+1

        final = ""
        if (j==bound):
            final="\n".join(lines[:i])+"\n      }\n"
        else:
            final = info
        print(final)
        proto.transport.write(final.encode())
        
        

class ClientProtocol(asyncio.Protocol):
    def __init__(self, message, loop):
        self.message = message
        self.loop = loop

    def connection_made(self, transport):
        transport.write(self.message.encode())
        print('Connection made: {!r}'.format(self.message))

        
        
class ServerProtocol(asyncio.Protocol):
    def __init__(self, serv_id, loop):
        self.serv_id=serv_id
        self.loop = loop
        self.portnumber= portnumbers[serv_id]
        self.network = comm_network[serv_id]

    def connection_made(self, transport):
        self.transport = transport
        self.peername = transport.get_extra_info('peername') 
        print('Connection from {}'.format(self.peername))
        logging.basicConfig(filename=logs[self.serv_id],level=logging.INFO)
        logging.info('Connection from {}'.format(self.peername))

        

    def data_received(self, data):
        message = data.decode()
        print('Data received: {!r}'.format(message))
        logging.basicConfig(filename=logs[self.serv_id],level=logging.INFO)
        logging.info('Data received: {!r}'.format(message))
        
        size = len(message)
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
                if (float(words[3])<= float(server_info[client_id]['time_stamp'])):
                    return_message=messenger(client_id)
                else:
                    update_server(client_id, self.serv_id, words[2], words[3])
                    return_message=messenger(client_id)
                    asyncio.ensure_future(send_data(self.loop, self.serv_id, return_message))
            else:
                update_server(client_id, self.serv_id, words[2], words[3])
                return_message=messenger(client_id)
                asyncio.ensure_future(send_data(self.loop, self.serv_id, return_message))
                
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
            asyncio.ensure_future(called(server_info[client_id]['location'],radius,bound, self))

        elif(words[0]=="AT"):
            if (len(words)!=6): #arg lengh error
                error(message)
            client_id=words[3]
            if (server_info.get(client_id, -1)!=-1):
                 try:
                    if(float(words[5])<= float(server_info[client_id]['time_stamp'])):
                        return_message=messenger(client_id)
                        print('Send: {!r}'.format(return_message))
                        self.transport.write(return_message.encode())
                 except ValueError:
                    error(message)
            else:
                update_server(client_id, words[1], words[4], words[5])
                server_info[client_id]['time_diff']=words[2]
                return_message=messenger(client_id)
                print('Send: {!r}'.format(return_message))

                asyncio.ensure_future(send_data(self.loop, self.serv_id, return_message))

        else:
            error(message)




            
    def connection_lost(self, exc):
        print('Lost connection of {}'.format(self.peername))
        self.transport.close()

        
def main():

    if (len(sys.argv) <2):
        error(sys.argv)
    serv_id = sys.argv[1]
    loop = asyncio.get_event_loop()
    if (portnumbers.get(serv_id, -1)!=-1): 
        coro = loop.create_server(lambda:ServerProtocol(serv_id, loop), '0.0.0.0', portnumbers[serv_id])
    else:
       error(sys.argv)
       
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

