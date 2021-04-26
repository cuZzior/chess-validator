package cuzz.chessvalidator;

import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;

public class UserInputFile implements UserInput {
    private final LineNumberReader reader;

    public UserInputFile(String fileName) throws FileNotFoundException {
        FileReader fr = new FileReader(fileName);
        this.reader = new LineNumberReader(fr);
    }

    public int[] nextMove() throws IOException {
        String line = this.reader.readLine();
        if (line != null) {
            int[] move = new int[]{line.charAt(0) - 97, 56 - line.charAt(1), line.charAt(2) - 97, 56 - line.charAt(3)};
            return move;
        } else {
            return null;
        }
    }
}
