#!/usr/bin/env python3

IN = './levels.txt'
OUT = './levels'

def main():
	with open(IN) as f:
		content = f.readlines()
		content = filter(lambda x: x[0] != '\'', content)

		i = iter(content)

		try:	
			while True:
				l = next(i)
				
				level = ''

				if 'Level' in l:
					name = l.split(' ')[1][:-1]
					l = next(i)

					while l[0] != '\n':
						level += l
						l = next(i)

				save(level, name)
		except:
			save(level, name)

def save(level, name):
	name = 'microban' + name
	with open(OUT + '/' + name + '.lvl', "w") as text_file:
  	  level = name + '\n' + level
  	  text_file.write(level)

if __name__ == "__main__":
	main()