"""
json_parser.py

Created for: bendean
By: Scott B. Bradley (scott2b)
Fiverr gig completed on: May. 6, 2014

Overview:
Converts VoxGov JSON data into CSV. Expects a directory containing VoxGov
JSON files. 

Requirements:
Requires either Python 2.x with unicodecsv installed, or Python 3.x

To execute:

    python json_parser.py <source_directory> [<output_file>]

Source directory is required, output_file is optional

"""
import codecs
import json
import os
import sys
try:
    import unicodecsv as csv
except ImportError:
    import csv

OUTPUT_FILE = 'output.csv'


def process_file(infile, writer):
    print('Processing file: %s' % infile)
    with codecs.open(infile, encoding='utf-8') as infile:
        data = json.load(infile)
        for item in data:
            _id = item['id']
            description =  item['description']
            for source in item['secondarySource']:
                source_id = source['sourceId']
                name = source['name']
                party = source['party']
                writer.writerow([_id, description, source_id, name, party])


def process_files_in_directory(directory, outfile):
    with codecs.open(outfile, 'w') as outfile:
        writer = csv.writer(outfile)
        writer.writerow(["id", "description", "sourceID", "name", "party"])
        for f in os.listdir(directory):
            if f.endswith('.json'):
                process_file(f, writer)

USAGE = """
Usage:

    python json_parser.py <source_directory> [<output_file>]

Where source_directory is path to directory with input JSON files.
output_file is optional -- defaults to %s
File names must end with .json
""" % OUTPUT_FILE


if __name__=='__main__':
    try:
        directory = sys.argv[1]
    except IndexError:
        print(USAGE)
        sys.exit(0)
    if len(sys.argv) > 2:
        outfile = sys.argv[2]
    else:
        outfile = OUTPUT_FILE
    process_files_in_directory(directory, outfile)
