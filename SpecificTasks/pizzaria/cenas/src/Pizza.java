/**
 * I N F x 1 2 0
 *
 * DÈcrivez votre classe $CLASSNAME ici.
 *
 * @author (votre nom)
 * @version (une date)
 *
 * (votre code permanent)
 * (votre adresse de courriel)
 */

import java.util.*;

public class Pizza {
    private static String mealCart =
            "+-------------------------------------------------------------------------------------------------+\n"+
            "|                                  Le tabeau de repas(Super Combos)                               |\n" +
            "+----------+--------------------------------------------------------------------+-----------------+\n" +
            "|Le numéro | La description                                                     | Le prix unitaire|\n" +
            "|----------+--------------------------------------------------------------------+-----------------|\n" +
            "|     3    | 1 Moyenne Pizza 12\" + 1 Petite Poutine + 1 Pepsi(355ml)            |       20.49     |\n" +
            "|     4    | 1 Moyenne Pizza 12\" + 10 ailes de poulet + Frites + 1 Pepsi(355ml) |       21.99     |\n" +
            "|     8    | 1 Pizza Large 14\" + Moyenne Poutine + 1 Pepsi(355ml)               |       24.49     |\n" +
            "|     9    | 1 Pizza Large 14\" + 12 ailes de poulet + Frites + 1 Pepsi(355ml)   |       25.99     |\n" +
            "|    13    | 1 Pizza X-Large 16\" + 1 Grande Poutine + 1 Pepsi(355ml)            |       28.49     |\n" +
            "|    14    | 1 Pizza X-Large 16\" + 15 ailes de poulet + 1 Pepsi(355ml)          |       29.99     |\n" +
            "+-------------------------------------------------------------------------------------------------+\n";
    private static String restaurantAddress = "5252 Matata Street, Hakuna, QC";
    private static String restaurantNumber = "514 848-7733";

    //print a welcome message
    public static void Welcome() {
        System.out.println("WELCOME TO PIZZA POUT TOUS!\n");
    }

    //print a welcome message
    public static void Menu() {
        System.out.println(
                "1. Invoice an order\n" +
                "2. Display the number of meals sold by meal type and the total amount of all meals sold\n" +
                "3. Exiting the program\n");
    }


    public static void summary(){
        System.out.println("Here you can order any type of pizza you want!\n" +
                "You can consult your order details and ordered meals by meal number.\n\n");
    }

    //read int and validate de chosen option
    public static int ReadInt(){
        Scanner s = new Scanner(System.in);
        int n = -1;
        boolean valid = false;

        while (!valid) {
            try {
                String line = s.nextLine();
                n = Integer.parseInt(line);
            } catch (NumberFormatException nfe) {
                System.out.println("Invalid number!");
                System.exit(0);
            }
            if (n < 1 || n > 3)
                System.out.println("The chosen option is invalid!\n");
            else
                valid = true;
        }

        return n;
    }

    //read and validate a phone number
    public static String phoneNumber(){
        Scanner s = new Scanner(System.in);
        boolean valid = false,digit = true;
        String line = "";

        while (!valid) {
            line = s.nextLine();
            int i = 0;
            digit = true;

            for (char c : line.toCharArray()) {
                if (!Character.isDigit(c) && i != 3 && i != 7)
                    digit = false;
                i++;
            }

            if (line.length() != 12 || !digit || line.charAt(3) != ' ' || line.charAt(7) != '-')
                System.out.println("The phone number is invalid!\n");
            else valid = true;
        }

        return line;
    }

    //read and validate a costumer's address
    public static String address(){
        Scanner s = new Scanner(System.in);
        boolean valid = false;
        String line = "";

        while (!valid) {
            line = s.nextLine();

            if (line.length() < 10 || line.length() > 80)
                System.out.println("The customer's address is invalid!\n");
            else valid = true;
        }

        return line;
    }

    //read int and validate meal number
    public static int mealNumber(){
        Scanner s = new Scanner(System.in);
        int n = -1;
        boolean valid = false;

        while (!valid) {
            try {
                String line = s.nextLine();
                n = Integer.parseInt(line);
            } catch (NumberFormatException nfe) {
                System.out.println("Invalid number!");
                System.exit(0);
            }
            if (n != 3 && n != 4 && n != 8 && n != 9 && n != 13 && n != 14)
                System.out.println("The meal number is invalid!\n");
            else
                valid = true;
        }

        return n;
    }

    //read and validate pizza type
    public static String pizzaType(){
        Scanner s = new Scanner(System.in);
        boolean valid = false;
        String line = "";

        while (!valid) {
            line = s.nextLine();

            if (!line.toUpperCase().equals("V") && !line.toUpperCase().equals("F") && !line.toUpperCase().equals("T") &&
                    !line.toUpperCase().equals("E") && !line.toUpperCase().equals("M"))
                System.out.println("The kind of pizza is invalid!\n");
            else valid = true;
        }

        return line;
    }

    //read int and validate number of meals
    public static int numberMeals(){
        Scanner s = new Scanner(System.in);
        int n = -1;
        boolean valid = false;

        while (!valid) {
            try {
                String line = s.nextLine();
                n = Integer.parseInt(line);
            } catch (NumberFormatException nfe) {
                System.out.println("Invalid number!");
                System.exit(0);
            }
            if (n <= 0)
                System.out.println(" The number of meals is invalid!\n");
            else
                valid = true;
        }

        return n;
    }

    //read and validate oui/no answer
    public static boolean anotherMeal(){
        Scanner s = new Scanner(System.in);
        boolean valid = false;
        String line = "";

        while (!valid) {
            line = s.nextLine();

            if (!line.toUpperCase().equals("O") && !line.toUpperCase().equals("N"))
                System.out.println("The answer is invalid!\n");
            else valid = true;
        }

        return line.toUpperCase().equals("O");
    }

    //read and validate payment type
    public static String payment(){
        Scanner s = new Scanner(System.in);
        boolean valid = false;
        String line = "";

        while (!valid) {
            line = s.nextLine();

            if (!line.equals("C") && !line.equals("D") && !line.equals("R"))
                System.out.println("The payment method is invalid!\n");
            else valid = true;
        }

        return line;
    }

    //calculate de subtotal
    public static Double subTotal(int nr3,int nr4,int nr8,int nr9,int nr13,int nr14){
        return nr3 * 20.49 + nr4 * 21.99 + nr8 * 24.49 + nr9 * 25.99 + nr13 * 28.49 + nr14 * 29.99;
    }

    //calculate TPS Tax
    public static Double TPSTax(Double subTotal){
        return subTotal * 0.05;
    }

    //calculate TVQ Tax
    public static Double TVQTax(Double subTotal){
        return subTotal * 0.09975;
    }

    //calculate total
    public static Double total(Double subTotal,Double tps,Double tvq){
        return subTotal + tps + tvq;
    }

    //calculate invoice number
    public static int invoiceNr(int nr){
        return nr+1;
    }

    //calculate total of a meal number
    public static int totalMealNumber(int targetedMeal,int numberMeals,int enteredMeal,int totalMeals){
        if(targetedMeal == enteredMeal)
            return numberMeals + totalMeals;
        else
            return numberMeals;
    }

    //description of the meal, add the new meal description to the previous description
    public static String mealDescription(int mealNumber,int quantity,String pizzaType,String description){
        Double price = 20.49;
        if (mealNumber == 4)
            price = 21.99;
        else if(mealNumber == 8)
            price = 24.49;
        else if(mealNumber == 9)
            price = 25.99;
        else if(mealNumber == 13)
            price = 28.49;
        else if(mealNumber == 14)
            price = 29.99;

        String s = "|      " + mealNumber + "      |      " + mealDescription(pizzaType) + "     |     " + quantity+ "      |  " + price + "   |\n";

        return description.concat(s);
    }

    //calculate total of a meal type
    public static int totalMealType(int numberMeals,int totalMeals){
            return numberMeals + totalMeals;
    }

    //calculate total of all meals
    public static int totalMeals(int total,int previoustotal){
        return total + previoustotal;
    }

    //-------------------------------------------------------------------------------------------

    //print restaurant info
    public static void header(){
        System.out.println(
                "+--------------------------------------------------------------+\n" +
                "|                       " + "PIZZA POUR TOUS" + "                        |\n" +
                "+--------------------------------------------------------------+" +
                "\n| " + restaurantAddress + "     " + java.time.LocalDateTime.now() +
                "|\n| " + restaurantNumber + "                                                 |\n"+
                "+--------------------------------------------------------------+");
    }

    //option 1
    //this method is used to print the invoice, receive the client data, meal description, total)
    public static void invoice(int invoiceNr,String address,String phone,String payment,String description,Double subTotal,Double tps,Double tvq,Double amount) {
        header();
        System.out.println(
                "\nClient details:\n"+
                "Address: "+address+
                "\nPhone: "+ phone +
                "\n\nInvoice number: " + invoiceNr);

        System.out.println( description +
                "+-------------+----------------------+------------+----------+" +
                "\n                            SubTotal: " + subTotal +
                "\n                                 TPS: " + tps +
                "\n                                 TVQ: " + tvq +
                "\n                               Total: " + amount +
                "\nPayment method: " + paymentType(payment) +
                "\n+------------------------------------------------------------+\n" +
                "\nThank you and see you next time!\n");
    }

    //option 2
    //this method is used to print the option2, receive the total meals and meals by number
    //print the restaurante info, current date and meals
    public static void option2(int nr3,int nr4,int nr8,int nr9,int nr13,int nr14,int totalMeals) {
        header();
        System.out.println(
                "Meals by type:" +
                "\n#3: " + nr3 +
                "\n#4: " + nr4 +
                "\n#8: " + nr8 +
                "\n#9: " + nr9 +
                "\n#13: " + nr13 +
                "\n#14: " + nr14 +
                "\nTotal meals: " + totalMeals + "\n" +
                "+--------------------------------------------------------------+\n");
    }

    //math the meal type char with its description
    public static String mealDescription(String meal){
        String description;
        switch (meal.toUpperCase()){
            case "V":
                description = "Vegetarian ";
                break;
            case "F":
                description = "Cheese     ";
                break;
            case "T":
                description = "Any topping";
                break;
            case "E":
                description = "Spinnach   ";
                break;
            case "M":
                description = "Seafood    ";
                break;
            default:
                description = "Undefined  ";
                break;
        }
        return description;
    }

    //math the payment type char with its description
    public static String paymentType(String type){
        String description;
        switch (type){
            case "C":
                description = "Cash     ";
                break;
            case "D":
                description = "Debit    ";
                break;
            case "R":
                description = "Credit   ";
                break;
            default:
                description = "Undefined";
                break;
        }
        return description;
    }

    //main method
    public static void main (String[] params) {
        int invoiceNr = 0;
        int totalMeals = 0,nr3Total = 0,nr4Total = 0,nr8Total = 0,nr9Total = 0,nr13Total = 0,nr14Total = 0;;
        int option = 0;
        boolean anotherMeal = true;
        Welcome();
        summary();

        while (option != 3){
            Menu();
            option = ReadInt();
            switch (option) {
                case 1:
                    anotherMeal = true;
                    invoiceNr = invoiceNr(invoiceNr);
                    System.out.println("Phone number(NNN NNN-NNNN): ");
                    String phone = phoneNumber();
                    System.out.println("Address: ");
                    String address = address();
                    String description = "\n+-------------+----------------------+------------+----------+" +
                                         "\n| Meal number |      Pizza Type      |  Quantity  |   Price  |"+
                                         "\n+-------------+----------------------+------------+----------+\n";
                    int n3 = 0,n4 = 0,n8 = 0,n9 = 0,n13 = 0,n14 = 0;

                    //read meal orders until anotherMeal boolean is false
                    while (anotherMeal) {
                        System.out.println(mealCart);
                        System.out.println("Meal number: ");
                        int meal = mealNumber();
                        System.out.println("Pizza type: ");
                        String pizzaType = pizzaType();
                        System.out.println("Number of meals: ");
                        int numberMeals = numberMeals();
                        totalMeals = totalMeals(numberMeals,totalMeals);
                        description = mealDescription(meal,numberMeals,pizzaType,description);

                        if(meal == 3) {nr3Total = totalMealType(numberMeals,nr3Total);n3 = totalMealType(numberMeals,n3);}
                        else if(meal == 4) {nr4Total = totalMealType(numberMeals,nr4Total);n4 = totalMealType(numberMeals,n4);}
                        else if(meal == 8) {nr8Total = totalMealType(numberMeals,nr8Total);n8 = totalMealType(numberMeals,n8);}
                        else if(meal == 9) {nr9Total = totalMealType(numberMeals,nr9Total);n9 = totalMealType(numberMeals,n9);}
                        else if(mimport java.util.*;

                        public class Pizza {
                            private static String mealCart =
                                    "+-------------------------------------------------------------------------------------------------+\n"+
                                            "|                                  Le tabeau de repas(Super Combos)                               |\n" +
                                            "+----------+--------------------------------------------------------------------+-----------------+\n" +
                                            "|Le numéro | La description                                                     | Le prix unitaire|\n" +
                                            "|----------+--------------------------------------------------------------------+-----------------|\n" +
                                            "|     3    | 1 Moyenne Pizza 12\" + 1 Petite Poutine + 1 Pepsi(355ml)            |       20.49     |\n" +
                                            "|     4    | 1 Moyenne Pizza 12\" + 10 ailes de poulet + Frites + 1 Pepsi(355ml) |       21.99     |\n" +
                                            "|     8    | 1 Pizza Large 14\" + Moyenne Poutine + 1 Pepsi(355ml)               |       24.49     |\n" +
                                            "|     9    | 1 Pizza Large 14\" + 12 ailes de poulet + Frites + 1 Pepsi(355ml)   |       25.99     |\n" +
                                            "|    13    | 1 Pizza X-Large 16\" + 1 Grande Poutine + 1 Pepsi(355ml)            |       28.49     |\n" +
                                            "|    14    | 1 Pizza X-Large 16\" + 15 ailes de poulet + 1 Pepsi(355ml)          |       29.99     |\n" +
                                            "+-------------------------------------------------------------------------------------------------+\n";
                            private static String restaurantAddress = "5252 Matata Street, Hakuna, QC";
                            private static String restaurantNumber = "514 848-7733";

                            //print a welcome message
                            public static void Welcome() {
                                System.out.println("WELCOME TO PIZZA POUT TOUS!\n");
                            }

                            //print a welcome message
                            public static void Menu() {
                                System.out.println(
                                        "1. Invoice an order\n" +
                                                "2. Display the number of meals sold by meal type and the total amount of all meals sold\n" +
                                                "3. Exiting the program\n");
                            }


                            public static void summary(){
                                System.out.println("Here you can order any type of pizza you want!\n" +
                                        "You can consult your order details and ordered meals by meal number.\n\n");
                            }

                            //read int and validate de chosen option
                            public static int ReadInt(){
                                Scanner s = new Scanner(System.in);
                                int n = -1;
                                boolean valid = false;

                                while (!valid) {
                                    try {
                                        String line = s.nextLine();
                                        n = Integer.parseInt(line);
                                    } catch (NumberFormatException nfe) {
                                        System.out.println("Invalid number!");
                                        System.exit(0);
                                    }
                                    if (n < 1 || n > 3)
                                        System.out.println("The chosen option is invalid!\n");
                                    else
                                        valid = true;
                                }

                                return n;
                            }

                            //read and validate a phone number
                            public static String phoneNumber(){
                                Scanner s = new Scanner(System.in);
                                boolean valid = false,digit = true;
                                String line = "";

                                while (!valid) {
                                    line = s.nextLine();
                                    int i = 0;
                                    digit = true;

                                    for (char c : line.toCharArray()) {
                                        if (!Character.isDigit(c) && i != 3 && i != 7)
                                            digit = false;
                                        i++;
                                    }

                                    if (line.length() != 12 || !digit || line.charAt(3) != ' ' || line.charAt(7) != '-')
                                        System.out.println("The phone number is invalid!\n");
                                    else valid = true;
                                }

                                return line;
                            }

                            //read and validate a costumer's address
                            public static String address(){
                                Scanner s = new Scanner(System.in);
                                boolean valid = false;
                                String line = "";

                                while (!valid) {
                                    line = s.nextLine();

                                    if (line.length() < 10 || line.length() > 80)
                                        System.out.println("The customer's address is invalid!\n");
                                    else valid = true;
                                }

                                return line;
                            }

                            //read int and validate meal number
                            public static int mealNumber(){
                                Scanner s = new Scanner(System.in);
                                int n = -1;
                                boolean valid = false;

                                while (!valid) {
                                    try {
                                        String line = s.nextLine();
                                        n = Integer.parseInt(line);
                                    } catch (NumberFormatException nfe) {
                                        System.out.println("Invalid number!");
                                        System.exit(0);
                                    }
                                    if (n != 3 && n != 4 && n != 8 && n != 9 && n != 13 && n != 14)
                                        System.out.println("The meal number is invalid!\n");
                                    else
                                        valid = true;
                                }

                                return n;
                            }

                            //read and validate pizza type
                            public static String pizzaType(){
                                Scanner s = new Scanner(System.in);
                                boolean valid = false;
                                String line = "";

                                while (!valid) {
                                    line = s.nextLine();

                                    if (!line.toUpperCase().equals("V") && !line.toUpperCase().equals("F") && !line.toUpperCase().equals("T") &&
                                            !line.toUpperCase().equals("E") && !line.toUpperCase().equals("M"))
                                        System.out.println("The kind of pizza is invalid!\n");
                                    else valid = true;
                                }

                                return line;
                            }

                            //read int and validate number of meals
                            public static int numberMeals(){
                                Scanner s = new Scanner(System.in);
                                int n = -1;
                                boolean valid = false;

                                while (!valid) {
                                    try {
                                        String line = s.nextLine();
                                        n = Integer.parseInt(line);
                                    } catch (NumberFormatException nfe) {
                                        System.out.println("Invalid number!");
                                        System.exit(0);
                                    }
                                    if (n <= 0)
                                        System.out.println(" The number of meals is invalid!\n");
                                    else
                                        valid = true;
                                }

                                return n;
                            }

                            //read and validate oui/no answer
                            public static boolean anotherMeal(){
                                Scanner s = new Scanner(System.in);
                                boolean valid = false;
                                String line = "";

                                while (!valid) {
                                    line = s.nextLine();

                                    if (!line.toUpperCase().equals("O") && !line.toUpperCase().equals("N"))
                                        System.out.println("The answer is invalid!\n");
                                    else valid = true;
                                }

                                return line.toUpperCase().equals("O");
                            }

                            //read and validate payment type
                            public static String payment(){
                                Scanner s = new Scanner(System.in);
                                boolean valid = false;
                                String line = "";

                                while (!valid) {
                                    line = s.nextLine();

                                    if (!line.equals("C") && !line.equals("D") && !line.equals("R"))
                                        System.out.println("The payment method is invalid!\n");
                                    else valid = true;
                                }

                                return line;
                            }

                            //calculate de subtotal
                            public static Double subTotal(int nr3,int nr4,int nr8,int nr9,int nr13,int nr14){
                                return nr3 * 20.49 + nr4 * 21.99 + nr8 * 24.49 + nr9 * 25.99 + nr13 * 28.49 + nr14 * 29.99;
                            }

                            //calculate TPS Tax
                            public static Double TPSTax(Double subTotal){
                                return subTotal * 0.05;
                            }

                            //calculate TVQ Tax
                            public static Double TVQTax(Double subTotal){
                                return subTotal * 0.09975;
                            }

                            //calculate total
                            public static Double total(Double subTotal,Double tps,Double tvq){
                                return subTotal + tps + tvq;
                            }

                            //calculate invoice number
                            public static int invoiceNr(int nr){
                                return nr+1;
                            }

                            //calculate total of a meal number
                            public static int totalMealNumber(int targetedMeal,int numberMeals,int enteredMeal,int totalMeals){
                                if(targetedMeal == enteredMeal)
                                    return numberMeals + totalMeals;
                                else
                                    return numberMeals;
                            }

                            //description of the meal, add the new meal description to the previous description
                            public static String mealDescription(int mealNumber,int quantity,String pizzaType,String description){
                                Double price = 20.49;
                                if (mealNumber == 4)
                                    price = 21.99;
                                else if(mealNumber == 8)
                                    price = 24.49;
                                else if(mealNumber == 9)
                                    price = 25.99;
                                else if(mealNumber == 13)
                                    price = 28.49;
                                else if(mealNumber == 14)
                                    price = 29.99;

                                String s = "|      " + mealNumber + "      |      " + mealDescription(pizzaType) + "     |     " + quantity+ "      |  " + price + "   |\n";

                                return description.concat(s);
                            }

                            //calculate total of a meal type
                            public static int totalMealType(int numberMeals,int totalMeals){
                                return numberMeals + totalMeals;
                            }

                            //calculate total of all meals
                            public static int totalMeals(int total,int previoustotal){
                                return total + previoustotal;
                            }

                            //-------------------------------------------------------------------------------------------

                            //print restaurant info
                            public static void header(){
                                System.out.println(
                                        "+--------------------------------------------------------------+\n" +
                                                "|                       " + "PIZZA POUR TOUS" + "                        |\n" +
                                                "+--------------------------------------------------------------+" +
                                                "\n| " + restaurantAddress + "     " + java.time.LocalDateTime.now() +
                                                "|\n| " + restaurantNumber + "                                                 |\n"+
                                                "+--------------------------------------------------------------+");
                            }

                            //option 1
                            //this method is used to print the invoice, receive the client data, meal description, total)
                            public static void invoice(int invoiceNr,String address,String phone,String payment,String description,Double subTotal,Double tps,Double tvq,Double amount) {
                                header();
                                System.out.println(
                                        "\nClient details:\n"+
                                                "Address: "+address+
                                                "\nPhone: "+ phone +
                                                "\n\nInvoice number: " + invoiceNr);

                                System.out.println( description +
                                        "+-------------+----------------------+------------+----------+" +
                                        "\n                            SubTotal: " + subTotal +
                                        "\n                                 TPS: " + tps +
                                        "\n                                 TVQ: " + tvq +
                                        "\n                               Total: " + amount +
                                        "\nPayment method: " + paymentType(payment) +
                                        "\n+------------------------------------------------------------+\n" +
                                        "\nThank you and see you next time!\n");
                            }

                            //option 2
                            //this method is used to print the option2, receive the total meals and meals by number
                            //print the restaurante info, current date and meals
                            public static void option2(int nr3,int nr4,int nr8,int nr9,int nr13,int nr14,int totalMeals) {
                                header();
                                System.out.println(
                                        "Meals by type:" +
                                                "\n#3: " + nr3 +
                                                "\n#4: " + nr4 +
                                                "\n#8: " + nr8 +
                                                "\n#9: " + nr9 +
                                                "\n#13: " + nr13 +
                                                "\n#14: " + nr14 +
                                                "\nTotal meals: " + totalMeals + "\n" +
                                                "+--------------------------------------------------------------+\n");
                            }

                            //math the meal type char with its description
                            public static String mealDescription(String meal){
                                String description;
                                switch (meal.toUpperCase()){
                                    case "V":
                                        description = "Vegetarian ";
                                        break;
                                    case "F":
                                        description = "Cheese     ";
                                        break;
                                    case "T":
                                        description = "Any topping";
                                        break;
                                    case "E":
                                        description = "Spinnach   ";
                                        break;
                                    case "M":
                                        description = "Seafood    ";
                                        break;
                                    default:
                                        description = "Undefined  ";
                                        break;
                                }
                                return description;
                            }

                            //math the payment type char with its description
                            public static String paymentType(String type){
                                String description;
                                switch (type){
                                    case "C":
                                        description = "Cash     ";
                                        break;
                                    case "D":
                                        description = "Debit    ";
                                        break;
                                    case "R":
                                        description = "Credit   ";
                                        break;
                                    default:
                                        description = "Undefined";
                                        break;
                                }
                                return description;
                            }

                            //main method
                            public static void main (String[] params) {
                                int invoiceNr = 0;
                                int totalMeals = 0,nr3Total = 0,nr4Total = 0,nr8Total = 0,nr9Total = 0,nr13Total = 0,nr14Total = 0;;
                                int option = 0;
                                boolean anotherMeal = true;
                                Welcome();
                                summary();

                                while (option != 3){
                                    Menu();
                                    option = ReadInt();
                                    switch (option) {
                                        case 1:
                                            anotherMeal = true;
                                            invoiceNr = invoiceNr(invoiceNr);
                                            System.out.println("Phone number(NNN NNN-NNNN): ");
                                            String phone = phoneNumber();
                                            System.out.println("Address: ");
                                            String address = address();
                                            String description = "\n+-------------+----------------------+------------+----------+" +
                                                    "\n| Meal number |      Pizza Type      |  Quantity  |   Price  |"+
                                                    "\n+-------------+----------------------+------------+----------+\n";
                                            int n3 = 0,n4 = 0,n8 = 0,n9 = 0,n13 = 0,n14 = 0;

                                            //read meal orders until anotherMeal boolean is false
                                            while (anotherMeal) {
                                                System.out.println(mealCart);
                                                System.out.println("Meal number: ");
                                                int meal = mealNumber();
                                                System.out.println("Pizza type: ");
                                                String pizzaType = pizzaType();
                                                System.out.println("Number of meals: ");
                                                int numberMeals = numberMeals();
                                                totalMeals = totalMeals(numberMeals,totalMeals);
                                                description = mealDescription(meal,numberMeals,pizzaType,description);

                                                if(meal == 3) {nr3Total = totalMealType(numberMeals,nr3Total);n3 = totalMealType(numberMeals,n3);}
                                                else if(meal == 4) {nr4Total = totalMealType(numberMeals,nr4Total);n4 = totalMealType(numberMeals,n4);}
                                                else if(meal == 8) {nr8Total = totalMealType(numberMeals,nr8Total);n8 = totalMealType(numberMeals,n8);}
                                                else if(meal == 9) {nr9Total = totalMealType(numberMeals,nr9Total);n9 = totalMealType(numberMeals,n9);}
                                                else if(meal == 13) {nr13Total = totalMealType(numberMeals,nr13Total);n13 = totalMealType(numberMeals,n13);}
                                                else if(meal == 14) {nr14Total = totalMealType(numberMeals,nr14Total);n14 = totalMealType(numberMeals,n14);}

                                                System.out.println("Another meal?(O/N)");
                                                anotherMeal = anotherMeal();
                                            }
                                            System.out.println("Payment:");
                                            String payment = payment();
                                            Double subTotal = subTotal(n3,n4,n8,n9,n13,n14),tps = TPSTax(subTotal),tvq = TVQTax(subTotal);

                                            invoice(invoiceNr,address,phone,payment,description,subTotal,tps,tvq,total(subTotal,tps,tvq));
                                            break;
                                        case 2:
                                            option2(nr3Total,nr4Total,nr8Total,nr9Total,nr13Total,nr14Total,totalMeals);
                                            break;
                                        case 3:
                                            System.out.println("Thank you and see you next time!");
                                            break;
                                        default: break;
                                    }
                                }
                            } // main
                        }eal == 13) {nr13Total = totalMealType(numberMeals,nr13Total);n13 = totalMealType(numberMeals,n13);}
                        else if(meal == 14) {nr14Total = totalMealType(numberMeals,nr14Total);n14 = totalMealType(numberMeals,n14);}

                        System.out.println("Another meal?(O/N)");
                        anotherMeal = anotherMeal();
                    }
                    System.out.println("Payment:");
                    String payment = payment();
                    Double subTotal = subTotal(n3,n4,n8,n9,n13,n14),tps = TPSTax(subTotal),tvq = TVQTax(subTotal);

                    invoice(invoiceNr,address,phone,payment,description,subTotal,tps,tvq,total(subTotal,tps,tvq));
                    break;
                case 2:
                    option2(nr3Total,nr4Total,nr8Total,nr9Total,nr13Total,nr14Total,totalMeals);
                    break;
                case 3:
                    System.out.println("Thank you and see you next time!");
                    break;
                default: break;
            }
        }
    } // main
}
