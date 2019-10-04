import asyncio
import aiohttp
import re
import sys
import json
import time
import logging


logging.basicConfig(filename= sys.argv[1] + ".log", format='%(asctime)s %(message)s', filemode='w', level=logging.DEBUG)

API_key = 'key'

#client name key, location 0, timestamp of message 1, server that received the message 2, timediff                                                                                                          
client_record = {}

inter_servers = {
    'Goloman': ['Hands', 'Holiday', 'Wilkes'],
    'Hands': ['Goloman', 'Wilkes'],
    'Holiday': ['Goloman', 'Welsh', 'Wilkes'],
    'Welsh': ['Holiday'],
    'Wilkes': ['Goloman', 'Hands', 'Holiday']
}

ports = { 'Goloman': 11751, 'Hands': 11752, 'Holiday': 11753, 'Welsh': 11754, 'Wilkes': 11755 }


def translate_coord(string):
    temp = string[1:].split('+')
    if len(temp) == 2:
        return str(float(string[0] + temp[0])) + "," + temp[1]
    else:
        temp = string[1:].split('-')
        return str(float(string[0] + temp[0])) + ",-" + temp[1]

def is_coordinates(string):
    return  re.fullmatch("[+-]\d+(\.\d+)?[+-]\d+(\.\d+)?", string)

def is_timestamp(string):
    return  re.fullmatch("\d+(\.\d+)?", string)

def is_timediff(string):
    return  re.fullmatch("[+-]\d+(\.\d+)?", string)

def is_valid(line):
    length = len(line)
    if length != 4 and length != 6:
        return -1
    else:
        if line[0] == "IAMAT":
            if length != 4 or (not is_coordinates(line[2])) or (not is_timestamp(line[3])):
                return -1
            else:
                return 0
        elif line[0] == "AT":
                return 1
        elif line[0] == "WHATSAT":
            if length != 4 or (not is_timestamp(line[2])) or float(line[2]) > 50 or float(line[2]) < 0 or (not re.fullmatch("\d+", line[3])) or int(line[3]) < 0 or int(line[3]) > 20:
                return -1
            else:
                return 2
        else:
            return -1


async def google_place(p1, p2, limit):
    result = ""
    async with aiohttp.ClientSession() as session:
        params =  [('location', p1), ('radius', p2), ('key', API_key)]
        async with session.get('https://maps.googleapis.com/maps/api/place/nearbysearch/json?', params = params) as resp:
            response = await resp.json()
            response['results'] = response['results'][:int(limit)]
            result += json.dumps(response, indent=3) + "\n\n"
            return result

async def propogate(cur_server, message):
    for server in inter_servers[cur_server]:
        try:
            reader, writer = await asyncio.open_connection('127.0.0.1', ports[server])
            writer.write(message.encode())
            writer.write_eof()
            await writer.drain()
            writer.close()
            logging.debug('SEND: %s TO %s', message, server)
        except ConnectionRefusedError:
            logging.debug('FAILED TO SEND: %s TO %s', message, server)
            pass
            
 async def update(message, cur_server):
    if message[3] not in client_record or message[5] > client_record[message[3]][1]:
        client_record[message[3]] = [message[4],message[5],message[1], message[2]]
        await propogate(cur_server, " ".join(message))


async def handle_connection(reader, writer):
    data = await reader.read()
    time_receive = time.time()
    message_recev = data.decode()
    logging.debug("RECEIVE: " + message_recev)
    line = message_recev.strip().split()

    check = is_valid(line)
    if check == 0:
        timediff = time_receive - float(line[3])
        if timediff >= 0:
            time_diff = "+" + str(timediff)
        else:
            time_diff = str(timediff)
        response = "AT {0} {1} {2} {3} {4}".format(sys.argv[1], time_diff, line[1], line[2], line[3])

        writer.write(response.encode())
        await writer.drain()
        await update(response.split(), sys.argv[1])
        logging.debug('SEND: %s', response)
    elif check == 1:
        await update(line, sys.argv[1])
    elif check == 2:
        if line[1] not in client_record:
            response = "? {0}".format(message_recev)
            writer.write(response.encode())
            await writer.drain()
            logging.debug('SEND: %s', response)
        else:
            record = client_record[line[1]]
            try:
                results = await google_place(translate_coord(client_record[line[1]][0]), line[2], line[3])
                response = "AT {0} {1} {2} {3} {4}".format(record[2], record[3], line[1], record[0], record[1]) + results
                writer.write(response.encode())
                await writer.drain()
                logging.debug('SEND: %s', response)
            except:
                logging.debug('FAILED TO SEARCH ABOUT %s', line[1])
                pass
    else:
       response = "? {0}".format(message_recev)
       writer.write(response.encode())
       await writer.drain()
       logging.debug('SEND: %s', response)

async def main():
    if len(sys.argv) != 2:
        print("Wrong argument number")
        sys.exit(1)
    if sys.argv[1] not in ports:
        print("Invalid server name")
        sys.exit(1)

    server = await asyncio.start_server(handle_connection, host='127.0.0.1', port=ports[sys.argv[1]])
    await server.serve_forever()

if __name__ == '__main__':
    asyncio.run(main())
