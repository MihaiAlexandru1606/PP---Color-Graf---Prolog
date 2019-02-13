from os import listdir
from os.path import isdir, join
from subprocess import call, check_output
import subprocess
import sys

def generate_output(swipl, tema, testin):
	# open test file
	with open(testin) as f:
		content = f.readlines()
		
	# remove endline characters
	content = [x.strip() for x in content] 
	
	# build prolog asserts
	for index in range(len(content)):
		line = content[index]
		assertline = 'assert(({0}))'.format(line[:-1])
		content[index] = assertline
		
	# concatenate assertions
	assertions = ','.join(content);
	
	# build goal
	checkingScript = '\
		from(From),\
		to(To),\
		input(Graph),\
		formula(Formula),\
		getPath(From, To, Graph, Formula, Path),\
		write(Path), halt\
	';
	
	# final prolog script
	prolog = assertions + ',' + checkingScript + '.';
		
	# run for 10 seconds max
	command = 'timeout 10 {0} -t halt -l "{1}" -g "{2}"'.format(swipl, tema, prolog)

	try:
		r = check_output(command, shell=True)
	except subprocess.CalledProcessError as grepexc:
		# return code 1 means goal failed
		# return code 2 means raised exception
		r = "false" if grepexc.returncode is 1 else "error"
	return r if r else "false"

if __name__ == '__main__':
	swipl_win = 'C:/Program\ Files/swipl/bin/swipl.exe'
	swipl = 'swipl'
	
	if (len(sys.argv) is not 3):
		print "Usage: python checker.py [path/to/fisiertema.pl] [path/to/testinput.txt]"
		sys.exit(0)
	
	tema = sys.argv[1]
	testin = sys.argv[2]

	output = generate_output(swipl, tema, testin)

	with open('output.txt', 'w') as f:
		f.write(output)
