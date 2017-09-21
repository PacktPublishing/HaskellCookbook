import xml.etree.ElementTree as ET

def walk(filename):
    context = ET.iterparse(filename, events=("end",))
    for action, e in context:
        ns = { 'w3' : 'http://www.w3.org/1999/xhtml' }
        for c in e.getchildren():
            print(dir(c))


if __name__ == '__main__':
    walk('index.html')
