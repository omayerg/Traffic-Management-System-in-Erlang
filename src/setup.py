import subprocess
import time
import pyautogui

def open_app_from_start_menu(app_name, command_to_execute):
    base_command = 'cd /mnt/c/Users/omgharra/Desktop/project/src'
    
    subprocess.Popen(['explorer', f'shell:AppsFolder\\{app_name}'])
    time.sleep(1.9)  # Adjust the delay as needed to ensure proper window launch

    # Simulate typing the base command, pressing Enter, and then typing the specific command
    pyautogui.typewrite(base_command, interval=0.01)
    pyautogui.press('enter')
    time.sleep(1)
    pyautogui.typewrite(command_to_execute, interval=0.01)
    pyautogui.press('enter')
    time.sleep(1)
    pyautogui.typewrite('c(cars).', interval=0.01)
    pyautogui.press('enter')
    time.sleep(0.5)  # Add a short delay to ensure proper execution
    pyautogui.typewrite('c(graphics).', interval=0.01)
    pyautogui.press('enter')
    time.sleep(0.5)  # Add a short delay to ensure proper execution
    pyautogui.typewrite('c(cars_manager).', interval=0.01)
    pyautogui.press('enter')
    time.sleep(0.5)  # Add a short delay to ensure proper execution
    pyautogui.typewrite('c(car_a_s).', interval=0.01)
    pyautogui.press('enter')
    time.sleep(0.5)  # Add a short delay to ensure proper execution
    pyautogui.typewrite('c(server).', interval=0.01)
    pyautogui.press('enter')
    time.sleep(0.5)  # Add a short delay to ensure proper execution
    pyautogui.typewrite('c(traffic_light).', interval=0.01)
    pyautogui.press('enter')
    time.sleep(0.5)  # Add a short delay to ensure proper execution
    pyautogui.typewrite('c(main).', interval=0.01)
    pyautogui.press('enter')
    time.sleep(0.5)
    pyautogui.typewrite('c(car_monitor).', interval=0.01)
    pyautogui.press('enter')
    
if __name__ == "__main__":
    num_windows = 5

    app_name = 'CanonicalGroupLimited.Ubuntu18.04LTS_79rhkp1fndgsc!ubuntu1804'

    commands_to_execute = [
        'erl -setcookie cooki -name NODE1@OMYR1.G',
        'erl -setcookie cooki -name NODE2@OMYR1.G',
        'erl -setcookie cooki -name NODE3@OMYR1.G',
        'erl -setcookie cooki -name NODE4@OMYR1.G',
        'erl -setcookie cooki -name master@OMYR1.G'
    ]

    # Open the app windows from the Start menu and run the corresponding command in each terminal
    for i in range(num_windows):
        full_command = f"{commands_to_execute[i]}"
        open_app_from_start_menu(app_name, full_command)
        time.sleep(1)  # Add an appropriate delay to ensure the previous window has enough time to open before opening the next one
