extends Camera2D

# Dragging / moving with mouse
var drag_speed:float = 1.0
var dragging: bool = false
var invert_axis: bool = true

func _input(event):
	# Mouse dragging
	if event is InputEventMouseButton:
		dragging = event.pressed

	if event is InputEventMouseMotion and dragging:
		var delta = event.relative
		var velocity = delta * drag_speed

		var mult = -1 if invert_axis else 1
		self.position.x += velocity[0] * mult
		self.position.y += velocity[1] * mult
