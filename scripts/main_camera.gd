extends Camera2D

# Dragging / moving with mouse
var drag_speed: float = 1.0
var dragging: bool = false
var invert_axis: bool = true

# Zooming with mouse wheel
var zoom_speed: float = 1.1

const  MIN_ZOOM = 0.5
const  MAX_ZOOM = 2

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

	# Zoom in/out with mouse wheel
	if event is InputEventMouseButton:
		if event.button_index == MOUSE_BUTTON_WHEEL_UP: # Zoom in
			zoom = zoom * zoom_speed

			if zoom.x > MAX_ZOOM: # Zoom should always be symmetrical, at least, let's hope so
				# Doing this because creating a new Vector2 with MIN_ZOOM it's slower
				zoom.x = MAX_ZOOM
				zoom.y = MAX_ZOOM

		elif event.button_index == MOUSE_BUTTON_WHEEL_DOWN: # Zoom out
			zoom = zoom / zoom_speed

			if zoom.x < MIN_ZOOM:
				zoom.x = MIN_ZOOM
				zoom.y = MIN_ZOOM
