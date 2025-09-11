def main():
    ws_prompt = "Please enter a name:"
    ws_greeting = "Hello, "
    ws_exclamation_point = "!"

    print(ws_prompt, end="")
    ws_friend = input()

    ws_friend_stripped = ws_friend.rstrip()

    ws_message = ws_greeting + ws_friend_stripped + ws_exclamation_point
    print(ws_message)

if __name__ == "__main__":
    main()