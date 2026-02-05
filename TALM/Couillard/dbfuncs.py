def explore(target):
	for obj in dir(target):
		print("%s: %s" %(obj, dir(obj)))
