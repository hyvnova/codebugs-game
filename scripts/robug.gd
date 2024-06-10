extends Area2D
class_name Robug

const SCENE = preload("res://scenes/robug.tscn")

enum State { 
	IDLE, 
	MOVING, # Persuing target_pos
}

# ---------------- ATTRIBUTES  ----------------  
var state = State.IDLE
var speed = 1
var velocity = 0
var target_pos: Vector2 # Holds the position robug is currently moving to


# Creating function, like a "new"
static func new_robug(pos: Vector2) -> Robug:
	var ins = SCENE.instantiate()
	ins.position = pos
	# Init vars and shi'
	return ins
	 
func go_to(pos: Vector2):
	state = State.MOVING
	target_pos = pos

	var dist = pos - self.position

	var duration = dist.length() / 200 * speed 
 
	var t = create_tween()
	t.tween_property(self, "position", pos, duration)
	t.tween_callback(func(): state = State.IDLE)

# Collision
func _on_area_entered(area):
	# When colliding with another robug
	if area.is_in_group("robug"):
		# Move away from the other robug
		var offset = self.position - area.position
		# Spacing
		offset = offset.normalized() * 10

		var t = create_tween()
		t.tween_property(self, "position", position + offset, 1)

		# If moving, callback will continue moving
		if state == State.MOVING:
			t.tween_callback(func(): go_to(target_pos))

