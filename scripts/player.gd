extends Area2D

@export var speed = 5
var velocity = 0
var  initial_scale = self.scale

var selected: InventoryItem  # current object selected

func wait(seconds: float) -> void:
	await get_tree().create_timer(seconds).timeout

func _ready():
	Global.set_player_reference(self)

func _input(event):
	if event.is_action_pressed("use"):
		print("using: ", selected)
		if selected and not selected.is_queued_for_deletion():
			# Pickup the item
			selected.pickup_item()
			selected = null

	# Command robugs to go to player position
	if event.is_action_pressed("accept"):
		for robug in get_tree().get_nodes_in_group("robug"):
			robug.go_to(self.global_position)

func _physics_process(_delta):
	# Keyboard movement
	var input_direction = Input.get_vector("left", "right", "up", "down")
	velocity = input_direction * speed
	self.position.x += velocity[0]
	self.position.y += velocity[1]

	# Make sure player doesn't go out of camera bounds
	var viewport = get_viewport().get_visible_rect()
	var h_width = viewport.size.x / 2
	var h_height = viewport.size.y / 2
	self.position.x = clamp(self.position.x, viewport.position.x - h_width, viewport.position.x + h_width)
	self.position.y = clamp(self.position.y, viewport.position.y - h_height, viewport.position.y + h_height)

# Collisions
func _on_area_entered(area: Area2D):

	# Origal idea for items
	if area.is_in_group("item_area"):
		velocity = 0 # Stop the player

		# Calculate the offset to move self to center the collided area
		var offset = area.global_position - global_position

		# This is fine because only InventoryItem will have item group uwU
		selected = area.get_parent() as InventoryItem 
		print("Selected: ", selected)
		# Tween self to center the collided area
		return await select(position + offset)

func _on_area_exited(_area):
	selected = null
	await unselect()

# When Colliding with an object
func select(end_pos):
	var tween = create_tween().set_parallel(true)
	tween.tween_property(self, "position", end_pos, 0.10)
	tween.tween_property(self, "rotation", 90, 0.5)
	tween.tween_property(self, "scale", initial_scale * 0.85, 0.5)

func unselect():
	var tween = create_tween().set_parallel(true)
	tween.tween_property(self, "rotation", 0, 0.5)
	tween.tween_property(self, "scale", initial_scale, 0.5)
