extends Area2D

@export var speed = 5
var velocity = 0
var initial_scale = self.scale
var selected: Area2D # current object selected

# Dragging / moving with mouse
var drag_speed: int = 2
var dragging: bool = false
var invert_axis: bool = true

func wait(seconds: float) -> void:
	await get_tree().create_timer(seconds).timeout

func _ready():
	Global.set_player_reference(self)

func _input(event):
	# Mouse dragging
	if event is InputEventMouseButton:
		dragging = event.pressed

	if event is InputEventMouseMotion and dragging:
		var delta = event.relative
		velocity = delta * drag_speed

		var mult = -1 if invert_axis else 1
		self.position.x += velocity[0] * mult
		self.position.y += velocity[1] * mult

func _physics_process(_delta):
	# Keyboard movement
	var input_direction = Input.get_vector("left", "right", "up", "down")
	velocity = input_direction * speed
	self.position.x += velocity[0]
	self.position.y += velocity[1]

# Collisions
func _on_area_entered(area: Area2D):
	if area.is_in_group("crap"):
		velocity = 0

		# Calculate the offset to move self to center the collided area
		var offset = area.global_position - global_position

		# Tween self to center the collided area
		return await select(position + offset)

func _on_area_exited(_area):
	await unselect()

# When Colliding with an object
func select(end_pos):
	var tween = create_tween().set_parallel(true)
	tween.tween_property(self, "position", end_pos, 0.10)
	tween.tween_property(self, "rotation", 90, 0.5)
	tween.tween_property(self, "scale", initial_scale * 0.85, 0.5)
	await wait(0.5)

func unselect():
	var tween = create_tween().set_parallel(true)
	tween.tween_property(self, "rotation", 0, 0.5)
	tween.tween_property(self, "scale", initial_scale, 0.5)
	await wait(0.5)
