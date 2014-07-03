
def highlight(string, status, face):
    attr = []
    if status == "green":
        # green
        attr.append('32')
    elif status == "red":
        # red
        attr.append('31')
    else:
        return string
    if face == "bold":
        attr.append('1')
    return '\x1b[%sm%s\x1b[0m' % (';'.join(attr), string)

