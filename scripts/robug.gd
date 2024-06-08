extends CharacterBody2D
class_name Robug

const SCENE = preload("res://scenes/robug.tscn")

# ---------------- ATTRIBUTES  ----------------  
var speed = 1

# Creating function, like a "new"
static func spawn(pos: Vector2) -> Robug:
	var ins = SCENE.instantiate()
	ins.position = pos
	# Init vars and shi'
	return ins
	 

func _physics_process(delta):
	var input_direction = Input.get_vector("left", "right", "up", "down")
	velocity = input_direction * speed
	self.position.x += velocity[0]
	self.position.y += velocity[1]
