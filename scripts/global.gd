### Global.gd
# ! AUTOLOADED as Global

extends Node

# Inventory items
var inventory: Array[ItemData] = [] # Each slot contains an InventoryItem or null. 

# Custom signals
signal inventory_updated
var spawnable_items = []

# Scene and node references
@onready var player_node: Node = Player


func _ready(): 
	# Initializes the inventory with 9 slots (spread over 3 blocks per row)
	inventory.resize(10) 

# on_pickup Adds an item to the inventory, returns true if successful
func add_item(item_data: ItemData) -> bool:

	# TODO! This is seriously slow, need to optimize this frfr ong
	for i in range(inventory.size()):
		# Check if a non-empty slot contains the same item type to stack.  
		if inventory[i] and inventory[i].type == item_data.type:
			inventory[i].quantity += item_data.quantity

			
				
			inventory_updated.emit()
			print("Item added (stack)", inventory)
			return true

		# Add the item to the first available slot
		elif inventory[i] == null:
			inventory[i] = item_data
			inventory_updated.emit()
			print("Item added", inventory)
			return true

	return false # Could not add the item

# Removes an item from the inventory
func remove_item(item_type) -> bool:
	for i in range(inventory.size()):
		# Find item with the same type and decrease the quantity
		if inventory[i] and inventory[i].type == item_type:
			inventory[i].quantity -= 1

			# If the quantity reaches zero, remove the item from the inventory
			if inventory[i].quantity <= 0:
				inventory[i] = null

			inventory_updated.emit()
			return true
	return false

# Increase inventory size dynamically
func increase_inventory_size(extra_slots):
	inventory.resize(inventory.size() + extra_slots)
	inventory_updated.emit()

# Sets the player reference for inventory interactions
func set_player_reference(player: Player):
	player_node = player

# Adjusts the drop position to avoid overlapping with nearby items
func adjust_drop_position(position: Vector2):
	var radius = 100
	var nearby_items = get_tree().get_nodes_in_group("item")
	for item in nearby_items:
		if item.global_position.distance_to(position) < radius:
			var random_offset = Vector2(randf_range(-radius, radius), randf_range(-radius, radius))
			position += random_offset
			break
	return position

# Drops an item at a specified position, adjusting for nearby items
func drop_item(item: ItemData, drop_position: Vector2):
	item.quantity = 1
	drop_position = adjust_drop_position(drop_position)
	World.spawn_item(item, drop_position)
	
