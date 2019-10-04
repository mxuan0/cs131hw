import asyncio
import time

async def tcp_echo_client(message, loop):
    reader, writer = await asyncio.open_connection('127.0.0.1', 11752, loop=loop)
    print("Sending:", message, end="")
    writer.write(message.encode())
    writer.write_eof()
    data = await reader.read(100000)
    print("Received:", data.decode(), end="")
    writer.close()

def main():
    message1 = "IAMAT kiwi.cs.ucla.edu -32.12+152.2 {0}\n".format(time.time())
    message = "WHATSAT kiwi.cs.ucla.edu 20 10\n"
    loop = asyncio.get_event_loop()
    loop.run_until_complete(tcp_echo_client(message, loop))
#    loop.run_until_complete(tcp_echo_client(message1, loop))                                                                                                                                               
#    loop.run_until_complete(tcp_echo_client(message, loop))                                                                                                                                                
    loop.close()

if __name__ == '__main__':
    main()
