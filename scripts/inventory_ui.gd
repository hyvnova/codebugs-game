### Inventory_UI.gd
extends Control

const SLOT_SCENE = preload("res://scenes/inventory_slot.tscn")

# Scene-Tree Node references
@onready var grid_container = $GridContainer

func _ready():
	# Connect function to signal to update inventory UI
	Global.inventory_updated.connect(_on_inventory_updated)
	_on_inventory_updated()

# TODO! This is slow. It re-creates the entire inventory UI every time the inventory is updated.
# * 	It should try to only update the slots that have changed.

# Update inventory UI
func _on_inventory_updated():
	# Clear existing slots
	clear_grid_container()
	# Add slots for each inventory position
	for item in Global.inventory:
		var slot = SLOT_SCENE.instantiate()
		grid_container.add_child(slot)

		if item:
			slot.set_item(item)
		else:
			slot.set_empty() 
				
# Clear inventory UI grid	
func clear_grid_container():
	while grid_container.get_child_count() > 0:
		var child = grid_container.get_child(0)
		grid_container.remove_child(child)
		child.queue_free()


func _input(event):
	if event.is_action_pressed("tab"):
		self.visible = not self.visible

func _on_open_inventory_btn_pressed():
	self.visible = not self.visible
